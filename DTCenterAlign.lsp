;;;==============================================================================;;;
;;                                                                               ;;
;;                    --=={  Text Center Line Align  }==--                       ;;
;;                                                                               ;;
;;  Places selected text object at the center of multiple selected lines/       ;;
;;  polylines. User can control offset distance, text side (Top/Bottom),        ;;
;;  perpendicularity, mirror, and text style.                                   ;;
;;                                                                               ;;
;;  Based on DTCurve by Lee McDonnell                                           ;;
;;  Modified for center alignment to multiple lines                             ;;
;;                                                                               ;;
;;  Features:                                                                    ;;
;;  - Select one TEXT or MTEXT object                                           ;;
;;  - Select multiple LINE or PLINE objects                                     ;;
;;  - Text placed at center of each line                                        ;;
;;  - Toggle side: T key (Top/Bottom) - Top = left side of line                ;;
;;  - Adjust offset: +/- keys or manual input                                   ;;
;;  - Toggle perpendicularity: P key                                            ;;
;;  - Mirror text: M key                                                        ;;
;;  - Change style/height: S key                                                ;;
;;  - Background mask (MTEXT only): B key                                       ;;
;;  - Real-time preview of all placements                                       ;;
;;                                                                               ;;
;;  Usage: Type DTCENTER to run                                                 ;;
;;                                                                               ;;
;;;==============================================================================;;;

(vl-load-com)

;;;==============================================================================;;;
;;                           Global Variables                                    ;;
;;;==============================================================================;;;

(setq *DTCA$OffsetMult* (if *DTCA$OffsetMult* *DTCA$OffsetMult* 0.4))
(setq *DTCA$Perp* (if *DTCA$Perp* *DTCA$Perp* T))
(setq *DTCA$Mirror* (if *DTCA$Mirror* *DTCA$Mirror* nil))
(setq *DTCA$Side* (if *DTCA$Side* *DTCA$Side* 1)) ; 1 = Top (left), -1 = Bottom (right)

;;;==============================================================================;;;
;;                           Utility Functions                                   ;;
;;;==============================================================================;;;

(defun *error* (msg)
  (if (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*"))
    (princ (strcat "\nError: " msg)))
  (redraw)
  (if doc (vla-EndUndoMark doc))
  (setvar "CMDECHO" cmd)
  (setvar "OSMODE" osm)
  (princ))

;; Get text content from TEXT or MTEXT
(defun GetTextContent (obj / typ)
  (setq typ (vla-get-ObjectName obj))
  (cond
    ((= typ "AcDbText") (vla-get-TextString obj))
    ((= typ "AcDbMText") (vla-get-TextString obj))
    (t "")))

;; Set text content for TEXT or MTEXT
(defun SetTextContent (obj str / typ)
  (setq typ (vla-get-ObjectName obj))
  (cond
    ((= typ "AcDbText") (vla-put-TextString obj str))
    ((= typ "AcDbMText") (vla-put-TextString obj str))))

;; Get text height
(defun GetTextHeight (obj / typ)
  (setq typ (vla-get-ObjectName obj))
  (cond
    ((= typ "AcDbText") (vla-get-Height obj))
    ((= typ "AcDbMText") (vla-get-Height obj))
    (t 2.5)))

;; Set text height
(defun SetTextHeight (obj h / typ)
  (setq typ (vla-get-ObjectName obj))
  (cond
    ((= typ "AcDbText") (vla-put-Height obj h))
    ((= typ "AcDbMText") (vla-put-Height obj h))))

;; Get center point of a line
(defun GetLineCenter (obj / sp ep)
  (setq sp (vlax-curve-getStartPoint obj)
        ep (vlax-curve-getEndPoint obj))
  (mapcar '(lambda (a b) (* 0.5 (+ a b))) sp ep))

;; Get line direction vector (normalized)
(defun GetLineDirection (obj / sp ep vec len)
  (setq sp (vlax-curve-getStartPoint obj)
        ep (vlax-curve-getEndPoint obj)
        vec (mapcar '- ep sp)
        len (distance sp ep))
  (if (> len 1e-6)
    (mapcar '(lambda (x) (/ x len)) vec)
    '(1.0 0.0 0.0)))

;; Get perpendicular vector (left side of line)
(defun GetPerpVector (dir)
  (list (- (cadr dir)) (car dir) 0.0))

;; Calculate text placement point
(defun CalcTextPoint (obj offset side perp / cen dir perp-vec ang)
  (setq cen (GetLineCenter obj)
        dir (GetLineDirection obj)
        ang (atan (cadr dir) (car dir)))
  
  (if perp
    (setq perp-vec (GetPerpVector dir))
    (setq perp-vec '(0.0 0.0 1.0)))
  
  (list
    (mapcar '(lambda (a b) (+ a (* offset side b))) cen perp-vec)
    (if perp ang 0.0)))

;; Text Style Dialog
(defun TextStyleDialog (currentStyle currentHeight / dcl_id ret style height)
  (setq style currentStyle
        height currentHeight)
  
  (if (setq dcl_id (load_dialog "DTCenterAlign.dcl"))
    (progn
      (if (not (new_dialog "textstyle" dcl_id))
        (progn
          (unload_dialog dcl_id)
          (setq ret nil))
        (progn
          (set_tile "style" currentStyle)
          (set_tile "height" (rtos currentHeight 2 3))
          
          (action_tile "accept"
            "(setq style (get_tile \"style\") height (atof (get_tile \"height\")) ret T) (done_dialog)")
          
          (action_tile "cancel"
            "(setq ret nil) (done_dialog)")
          
          (start_dialog)
          (unload_dialog dcl_id)
          
          (if ret
            (list style height)
            nil))))
    (progn
      (princ "\nDialog file not found.")
      nil)))

;;;==============================================================================;;;
;;                           Main Function                                       ;;
;;;==============================================================================;;;

(defun C:DTCENTER (/ doc cmd osm txt txtObj txtType txtStr txtStyle txtHeight
                      lines lineList ss i obj gr data key pt ang
                      offset perp mirror side previewList
                      origInsertPt origAlignPt origAlignment origAttach origRotation)
  
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        cmd (getvar "CMDECHO")
        osm (getvar "OSMODE"))
  
  (setvar "CMDECHO" 0)
  (vla-StartUndoMark doc)
  
  ;; Select text object
  (princ "\nSelect TEXT or MTEXT object: ")
  (if (setq txt (ssget "_+.:S:E" '((0 . "TEXT,MTEXT"))))
    (progn
      (setq txtObj (vlax-ename->vla-object (ssname txt 0))
            txtType (vla-get-ObjectName txtObj)
            txtStr (GetTextContent txtObj)
            txtStyle (vla-get-StyleName txtObj)
            txtHeight (GetTextHeight txtObj))

      (princ (strcat "\nText selected: \"" txtStr "\" (" txtType ", Height=" (rtos txtHeight 2 2) ")"))

      ;; Save original text state for cancel-restore
      (setq origInsertPt (vlax-get txtObj 'InsertionPoint)
            origRotation (vla-get-Rotation txtObj))
      (if (= txtType "AcDbText")
        (setq origAlignment (vla-get-Alignment txtObj)
              origAlignPt (vlax-get txtObj 'TextAlignmentPoint))
        (setq origAttach (vla-get-AttachmentPoint txtObj)))

      ;; Select lines/polylines (text stays in place until confirmed)
      (princ "\nSelect LINE or PLINE objects: ")
      (if (setq lines (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
        (progn
          (setq lineList '()
                i 0)
          
          ;; Build line list
          (repeat (sslength lines)
            (setq obj (vlax-ename->vla-object (ssname lines i))
                  lineList (cons obj lineList)
                  i (1+ i)))
          
          (setq lineList (reverse lineList))
          
          ;; Initialize settings (offset as multiplier of text height)
          (setq offset (* txtHeight *DTCA$OffsetMult*)
                perp *DTCA$Perp*
                mirror *DTCA$Mirror*
                side *DTCA$Side*)          

          ;; Now change justification (deferred until lines are selected)
          ;; side=1 (Top) means text sits above line -> BottomCenter anchor
          ;; side=-1 (Bottom) means text sits below line -> TopCenter anchor
          (if (= txtType "AcDbText")
            (progn
              (vla-put-Alignment txtObj (if (= side 1) acAlignmentBottomCenter acAlignmentTopCenter))
              (vla-put-TextAlignmentPoint txtObj (vla-get-InsertionPoint txtObj)))
            (vla-put-AttachmentPoint txtObj (if (= side 1) acAttachmentPointBottomCenter acAttachmentPointTopCenter)))
          
          (princ "\n[T] Toggle side | [+/-] Offset | [P] Perpendicular | [M] Mirror | [S] Style | [B] Mask | [Enter] Accept | [Esc] Cancel")
          (princ (strcat "\nCurrent: Side=" (if (= side 1) "Top" "Bottom") 
                        " | Offset=" (rtos offset 2 2)
                        " | Perp=" (if perp "ON" "OFF")
                        " | Mirror=" (if mirror "ON" "OFF")))
          
          ;; Function to update preview
          (defun UpdatePreview (/ lineObj data pt ang tempObj)
            ;; Clear previous grdraw vectors and highlights
            (redraw)
            ;; Highlight the source text object
            (redraw (vlax-vla-object->ename txtObj) 3)
            ;; Draw preview indicators on all lines
            (foreach lineObj lineList
              (setq data (CalcTextPoint lineObj offset side perp)
                    pt (car data)
                    ang (cadr data))
              
              (if mirror
                (setq ang (+ ang pi)))
              
              ;; Draw small cross at text position (scaled to text height)
              (grdraw
                (list (- (car pt) (* txtHeight 0.5)) (cadr pt) (caddr pt))
                (list (+ (car pt) (* txtHeight 0.5)) (cadr pt) (caddr pt))
                3 1)
              (grdraw
                (list (car pt) (- (cadr pt) (* txtHeight 0.5)) (caddr pt))
                (list (car pt) (+ (cadr pt) (* txtHeight 0.5)) (caddr pt))
                3 1))
            
            ;; Update first text for preview
            (setq data (CalcTextPoint (car lineList) offset side perp)
                  pt (car data)
                  ang (cadr data))
            
            (if mirror
              (setq ang (+ ang pi)))
            
            (if (= txtType "AcDbText")
              (progn
                (vla-put-TextAlignmentPoint txtObj (vlax-3d-point pt))
                (vla-put-Rotation txtObj ang))
              (progn
                (vla-put-InsertionPoint txtObj (vlax-3d-point pt))
                (vla-put-Rotation txtObj ang)))
            
            (vla-Update txtObj))
          
          ;; Initial preview
          (UpdatePreview)
          (setvar "OSMODE" 0)
          
          ;; Preview loop with continuous update
          ;; Exit on Enter(13), Space(32), Escape(27), or right-click(25)
          (setq gr nil)
          (while (not (member (setq gr (grread t 15 0)) '((2 13) (2 32) (2 27))))
            
            (cond
              ;; Mouse movement - just redraw preview
              ((= (car gr) 5)
               (UpdatePreview))
              
              ;; Toggle Side (T key) - also switches text anchor between BottomCenter and TopCenter
              ((member gr '((2 84) (2 116)))
               (setq side (- side))
               (if (= txtType "AcDbText")
                 (vla-put-Alignment txtObj (if (= side 1) acAlignmentBottomCenter acAlignmentTopCenter))
                 (vla-put-AttachmentPoint txtObj (if (= side 1) acAttachmentPointBottomCenter acAttachmentPointTopCenter)))
               (UpdatePreview)
               (princ (strcat "\rSide: " (if (= side 1) "Top (Left)" "Bottom (Right)") " | Offset: " (rtos offset 2 2) "          ")))
              
;; Increase Offset (+)
((member gr '((2 43) 43))
  (setq offset (+ offset (* txtHeight 0.1)))
  (UpdatePreview)
  (princ (strcat "\rOffset: " (rtos offset 2 2) "          ")))

;; Decrease Offset (-)
((member gr '((2 45) 45))
  (setq offset (max 0.0 (- offset (* txtHeight 0.1))))
  (UpdatePreview)
  (princ (strcat "\rOffset: " (rtos offset 2 2) "          ")))
              
              ;; Manual offset input (O key)
              ((member gr '((2 79) (2 111)))
               (setvar "OSMODE" osm)
               (if (setq data (getdist (strcat "\nEnter offset distance <" (rtos offset 2 2) ">: ")))
                 (setq offset data))
               (setvar "OSMODE" 0)
               (UpdatePreview)
               (princ (strcat "\rOffset: " (rtos offset 2 2) "          ")))
              
              ;; Toggle Perpendicularity (P key)
              ((member gr '((2 80) (2 112)))
               (setq perp (not perp))
               (UpdatePreview)
               (princ (strcat "\rPerpendicular: " (if perp "ON" "OFF") "          ")))
              
              ;; Toggle Mirror (M key)
              ((member gr '((2 77) (2 109)))
               (setq mirror (not mirror))
               (UpdatePreview)
               (princ (strcat "\rMirror: " (if mirror "ON" "OFF") "          ")))
              
              ;; Style/Height Dialog (S key)
              ((member gr '((2 83) (2 115)))
               (setvar "OSMODE" osm)
               (if (setq data (TextStyleDialog txtStyle txtHeight))
                 (progn
                   (setq txtStyle (car data)
                         txtHeight (cadr data))
                   (vla-put-StyleName txtObj txtStyle)
                   (SetTextHeight txtObj txtHeight)
                   (UpdatePreview)
                   (princ (strcat "\rStyle: " txtStyle " | Height: " (rtos txtHeight 2 2) "          "))))
               (setvar "OSMODE" 0))
              
              ;; Toggle Background Mask (B key) - MTEXT only
              ((and (= txtType "AcDbMText") (member gr '((2 66) (2 98))))
               (vla-put-BackgroundFill txtObj (if (= (vla-get-BackgroundFill txtObj) :vlax-true) :vlax-false :vlax-true))
               (UpdatePreview)
               (princ (strcat "\rBackground Mask: " (if (= (vla-get-BackgroundFill txtObj) :vlax-true) "ON" "OFF") "          ")))))
          
          (setvar "OSMODE" osm)
          ;; Clear all grdraw crosses
          (redraw)

          ;; Check how the loop exited
          (if (equal gr '(2 27))
            ;; Cancelled (Escape) - restore original text state
            (progn
              (if (= txtType "AcDbText")
                (progn
                  (vla-put-Alignment txtObj origAlignment)
                  (vlax-put txtObj 'InsertionPoint origInsertPt)
                  (vlax-put txtObj 'TextAlignmentPoint origAlignPt))
                (progn
                  (vla-put-AttachmentPoint txtObj origAttach)
                  (vlax-put txtObj 'InsertionPoint origInsertPt)))
              (vla-put-Rotation txtObj origRotation)
              (vla-Update txtObj)
              (princ "\n*Cancelled - text restored*"))

            ;; Confirmed (Enter/Space) - apply to all lines
            (progn
              (princ "\n\nApplying text to all lines...")
              (setq i 0)

              (foreach lineObj lineList
                (setq data (CalcTextPoint lineObj offset side perp)
                      pt (car data)
                      ang (cadr data))

                (if mirror
                  (setq ang (+ ang pi)))

                (if (> i 0)
                  ;; Create copy for additional lines
                  (setq txtObj (vla-Copy txtObj)))

                ;; Set position and rotation
                (if (= txtType "AcDbText")
                  (progn
                    (vla-put-TextAlignmentPoint txtObj (vlax-3d-point pt))
                    (vla-put-Rotation txtObj ang))
                  (progn
                    (vla-put-InsertionPoint txtObj (vlax-3d-point pt))
                    (vla-put-Rotation txtObj ang)))

                (vla-Update txtObj)
                (setq i (1+ i)))

              ;; Save settings (store offset as multiplier of text height)
              (setq *DTCA$OffsetMult* (/ offset txtHeight)
                    *DTCA$Perp* perp
                    *DTCA$Mirror* mirror
                    *DTCA$Side* side)

              (princ (strcat "\n" (itoa (length lineList)) " text objects created/updated.")))))
        
        (princ "\n*Cancel*")))
    (princ "\n*Cancel*"))

  ;; Clear any remaining grdraw vectors/highlights
  (redraw)
  (vla-EndUndoMark doc)
  (setvar "CMDECHO" cmd)
  (setvar "OSMODE" osm)
  (princ))

;;;==============================================================================;;;

(princ "\n*** DTCenterAlign loaded. Type DTCENTER to run ***")
(princ)

;;;==============================================================================;;;
;;                             End of Program                                    ;;
;;;==============================================================================;;;
