;;; ============================================================================
;;; COORDRND_v1.1.lsp
;;; 
;;; Description: Rounds coordinates of selected objects (lines, polylines, arcs,
;;;              splines, blocks, text, dimensions, leaders, hatches) relative 
;;;              to a specified base point
;;;
;;; Command: COORDRND
;;;
;;; Version: 1.1
;;; Date: 2026-01-15
;;; Author: Dmitrii
;;; 
;;; Usage: Type COORDRND at command line, select objects, specify base point
;;;        and rounding precision
;;; 
;;; Changelog v1.1:
;;; - Fixed warning symbol issue in prompts
;;; - Added separate count for total vs supported objects
;;; - Added support for TEXT, MTEXT, DIMENSION, LEADER, HATCH objects
;;; ============================================================================

(defun C:COORDRND (/ *error* old-vars ss ss-all base-pt round-val 
                    count-total count-supported count-processed obj idx ent entdata
                    obj-type result supported-types)
  
  ;; ============================================================================
  ;; CONFIGURABLE DEFAULT VALUES - Modify these as needed
  ;; ============================================================================
  
  (setq *COORDRND-DEFAULT-ROUND* 10.0)        ; Default rounding value
  (setq *COORDRND-DEFAULT-BASE-X* 0.0)       ; Default base point X
  (setq *COORDRND-DEFAULT-BASE-Y* 0.0)       ; Default base point Y
  (setq *COORDRND-DEFAULT-BASE-Z* 0.0)       ; Default base point Z
  
  ;; Define supported object types for filtering
  (setq supported-types '("LINE" "LWPOLYLINE" "POLYLINE" "ARC" "SPLINE" "INSERT"
                          "TEXT" "MTEXT" "DIMENSION" "LEADER" "HATCH"))
  
  ;; ============================================================================
  ;; ERROR HANDLER - Restores system variables on error
  ;; ============================================================================
  
  (defun *error* (msg)
    (if old-vars
      (progn
        (setvar "CMDECHO" (car old-vars))
        (setvar "OSMODE" (cadr old-vars))
      )
    )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  
  ;; ============================================================================
  ;; MAIN PROGRAM START
  ;; ============================================================================
  
  (princ "\n=== COORDRND v1.0 - Coordinate Rounding Tool ===\n")
  
  ;; Save system variables
  (setq old-vars (list (getvar "CMDECHO") (getvar "OSMODE")))
  (setvar "CMDECHO" 0)
  
  ;; ============================================================================
  ;; USER INPUT - Object Selection
  ;; ============================================================================
  
  (princ "\nSelect objects to round coordinates:")
  (princ "\n  Supported: LINE, PLINE, LWPOLYLINE, ARC, SPLINE, INSERT (blocks),")
  (princ "\n             TEXT, MTEXT, DIMENSION, LEADER, HATCH")
  
  ;; Select all objects first (no filter)
  (setq ss-all (ssget))
  
  (if (null ss-all)
    (progn
      (*error* "No objects selected")
      (exit)
    )
  )
  
  ;; Count total selected objects
  (setq count-total (sslength ss-all))
  (princ (strcat "\nTotal objects selected: " (itoa count-total)))
  
  ;; Filter to get only supported object types
  (setq ss (ssget "_P" '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE,INSERT,TEXT,MTEXT,DIMENSION,LEADER,HATCH"))))
  
  (if (null ss)
    (progn
      (princ "\nError: No supported objects found in selection")
      (*error* "No supported objects")
      (exit)
    )
  )
  
  ;; Count supported objects
  (setq count-supported (sslength ss))
  (princ (strcat "\nSupported objects: " (itoa count-supported)))
  
  ;; Show how many objects are ignored
  (if (> count-total count-supported)
    (princ (strcat "\nIgnored (unsupported): " (itoa (- count-total count-supported))))
  )
  
  ;; ============================================================================
  ;; USER INPUT - Base Point
  ;; ============================================================================
  
  (princ (strcat "\nSpecify base point for rounding <"
                 (rtos *COORDRND-DEFAULT-BASE-X* 2 3) ","
                 (rtos *COORDRND-DEFAULT-BASE-Y* 2 3) ","
                 (rtos *COORDRND-DEFAULT-BASE-Z* 2 3) ">: "))
  
  (setq base-pt (getpoint))
  
  ;; Use default if user pressed Enter
  (if (null base-pt)
    (setq base-pt (list *COORDRND-DEFAULT-BASE-X* 
                        *COORDRND-DEFAULT-BASE-Y* 
                        *COORDRND-DEFAULT-BASE-Z*))
  )
  
  (princ (strcat "\nBase point: " 
                 (rtos (car base-pt) 2 3) ","
                 (rtos (cadr base-pt) 2 3) ","
                 (rtos (caddr base-pt) 2 3)))
  
  ;; ============================================================================
  ;; USER INPUT - Rounding Value
  ;; ============================================================================
  
  ;; Use initget to prevent zero and negative values, but allow null
  (initget (+ 2 4))  ; Bit 2 = no zero, Bit 4 = no negative
  
  (setq round-val (getdist (strcat "\nSpecify rounding precision or press Enter for <" 
                                   (rtos *COORDRND-DEFAULT-ROUND* 2 3) ">: ")))
  
  ;; Use default if user pressed Enter
  (if (null round-val)
    (setq round-val *COORDRND-DEFAULT-ROUND*)
  )
  
  (princ (strcat "\nRounding precision: " (rtos round-val 2 6)))
  
  ;; ============================================================================
  ;; PROCESSING LOOP - Process each selected object
  ;; ============================================================================
  
  (princ "\n\nProcessing objects...")
  (setq count-processed 0)
  (setq idx 0)
  
  ;; Iterate through selection set
  (repeat count-supported
    (setq ent (ssname ss idx))
    (setq entdata (entget ent))
    (setq obj-type (cdr (assoc 0 entdata)))
    
    ;; Process based on object type
    (cond
      ;; ========================================================================
      ;; LINE - Round start and end points
      ;; ========================================================================
      ((= obj-type "LINE")
       (setq result (round-line ent entdata base-pt round-val))
       (if result (setq count-processed (1+ count-processed)))
      )
      
      ;; ========================================================================
      ;; LWPOLYLINE - Round all vertices
      ;; ========================================================================
      ((= obj-type "LWPOLYLINE")
       (setq result (round-lwpolyline ent entdata base-pt round-val))
       (if result (setq count-processed (1+ count-processed)))
      )
      
      ;; ========================================================================
      ;; POLYLINE (Legacy) - Round all vertices
      ;; ========================================================================
      ((= obj-type "POLYLINE")
       (setq result (round-polyline ent base-pt round-val))
       (if result (setq count-processed (1+ count-processed)))
      )
      
      ;; ========================================================================
      ;; ARC - Round center point
      ;; ========================================================================
      ((= obj-type "ARC")
       (setq result (round-arc ent entdata base-pt round-val))
       (if result (setq count-processed (1+ count-processed)))
      )
      
      ;; ========================================================================
      ;; SPLINE - Round control points
      ;; ========================================================================
      ((= obj-type "SPLINE")
       (setq result (round-spline ent entdata base-pt round-val))
       (if result (setq count-processed (1+ count-processed)))
      )
      
      ;; ========================================================================
      ;; INSERT (Block) - Round insertion point
      ;; ========================================================================
      ((= obj-type "INSERT")
       (setq result (round-insert ent entdata base-pt round-val))
       (if result (setq count-processed (1+ count-processed)))
      )
      
      ;; ========================================================================
      ;; TEXT - Round insertion point
      ;; ========================================================================
      ((= obj-type "TEXT")
       (setq result (round-text ent entdata base-pt round-val))
       (if result (setq count-processed (1+ count-processed)))
      )
      
      ;; ========================================================================
      ;; MTEXT - Round insertion point
      ;; ========================================================================
      ((= obj-type "MTEXT")
       (setq result (round-mtext ent entdata base-pt round-val))
       (if result (setq count-processed (1+ count-processed)))
      )
      
      ;; ========================================================================
      ;; DIMENSION - Round definition points
      ;; ========================================================================
      ((= obj-type "DIMENSION")
       (setq result (round-dimension ent entdata base-pt round-val))
       (if result (setq count-processed (1+ count-processed)))
      )
      
      ;; ========================================================================
      ;; LEADER - Round vertices
      ;; ========================================================================
      ((= obj-type "LEADER")
       (setq result (round-leader ent entdata base-pt round-val))
       (if result (setq count-processed (1+ count-processed)))
      )
      
      ;; ========================================================================
      ;; HATCH - Round seed point (base point)
      ;; ========================================================================
      ((= obj-type "HATCH")
       (setq result (round-hatch ent entdata base-pt round-val))
       (if result (setq count-processed (1+ count-processed)))
      )
    )
    
    (setq idx (1+ idx))
  )
  
  ;; ============================================================================
  ;; COMPLETION - Display results
  ;; ============================================================================
  
  (princ "\n\n=== COORDRND Results ===")
  (princ (strcat "\nTotal selected: " (itoa count-total)))
  (princ (strcat "\nSupported objects: " (itoa count-supported)))
  (princ (strcat "\nProcessed objects: " (itoa count-processed)))
  (if (> count-supported count-processed)
    (princ (strcat "\nFailed to process: " (itoa (- count-supported count-processed))))
  )
  (if (> count-total count-supported)
    (princ (strcat "\nIgnored (unsupported): " (itoa (- count-total count-supported))))
  )
  (princ "\n========================\n")
  
  ;; Restore system variables
  (setvar "CMDECHO" (car old-vars))
  (setvar "OSMODE" (cadr old-vars))
  
  (princ)
)

;; ============================================================================
;; UTILITY FUNCTION - Round a single coordinate value
;; ============================================================================
;; Takes: coord-val (coordinate value), base-val (base coordinate), 
;;        round-val (rounding precision)
;; Returns: Rounded coordinate value
;; ============================================================================

(defun round-coordinate (coord-val base-val round-val / relative-val rounded-relative)
  ;; Calculate coordinate relative to base point
  (setq relative-val (- coord-val base-val))
  
  ;; Round to nearest multiple of round-val
  (setq rounded-relative (* round-val 
                           (fix (+ (/ relative-val round-val) 0.5))))
  
  ;; Return absolute coordinate
  (+ base-val rounded-relative)
)

;; ============================================================================
;; UTILITY FUNCTION - Round a point (list of coordinates)
;; ============================================================================
;; Takes: pt (point as list), base-pt (base point), round-val (precision)
;; Returns: Rounded point
;; ============================================================================

(defun round-point (pt base-pt round-val)
  (list
    (round-coordinate (car pt) (car base-pt) round-val)
    (round-coordinate (cadr pt) (cadr base-pt) round-val)
    (if (caddr pt)
      (round-coordinate (caddr pt) (caddr base-pt) round-val)
      0.0
    )
  )
)

;; ============================================================================
;; OBJECT PROCESSOR - LINE
;; ============================================================================
;; Rounds the start point (10) and end point (11) of a line
;; ============================================================================

(defun round-line (ent entdata base-pt round-val / start-pt end-pt new-start new-end)
  (setq start-pt (cdr (assoc 10 entdata)))
  (setq end-pt (cdr (assoc 11 entdata)))
  
  (setq new-start (round-point start-pt base-pt round-val))
  (setq new-end (round-point end-pt base-pt round-val))
  
  ;; Update entity data
  (setq entdata (subst (cons 10 new-start) (assoc 10 entdata) entdata))
  (setq entdata (subst (cons 11 new-end) (assoc 11 entdata) entdata))
  
  ;; Apply changes to drawing
  (entmod entdata)
  (entupd ent)
  T ; Return success
)

;; ============================================================================
;; OBJECT PROCESSOR - LWPOLYLINE
;; ============================================================================
;; Rounds all vertices (10) of a lightweight polyline
;; ============================================================================

(defun round-lwpolyline (ent entdata base-pt round-val / vertex new-vertex new-entdata)
  (setq new-entdata entdata)
  
  ;; Process each vertex (DXF code 10)
  (foreach item entdata
    (if (= (car item) 10)
      (progn
        (setq vertex (cdr item))
        (setq new-vertex (round-point vertex base-pt round-val))
        (setq new-entdata (subst (cons 10 new-vertex) item new-entdata))
      )
    )
  )
  
  ;; Apply changes to drawing
  (entmod new-entdata)
  (entupd ent)
  T ; Return success
)

;; ============================================================================
;; OBJECT PROCESSOR - POLYLINE (Legacy)
;; ============================================================================
;; Rounds all vertices of a legacy polyline (processes vertex entities)
;; ============================================================================

(defun round-polyline (ent base-pt round-val / vertex-ent vertex-data pt new-pt)
  ;; Get first vertex
  (setq vertex-ent (entnext ent))
  
  ;; Loop through all vertices
  (while (and vertex-ent 
              (= (cdr (assoc 0 (entget vertex-ent))) "VERTEX"))
    (setq vertex-data (entget vertex-ent))
    (setq pt (cdr (assoc 10 vertex-data)))
    
    ;; Round vertex coordinates
    (setq new-pt (round-point pt base-pt round-val))
    
    ;; Update vertex
    (setq vertex-data (subst (cons 10 new-pt) (assoc 10 vertex-data) vertex-data))
    (entmod vertex-data)
    (entupd vertex-ent)
    
    ;; Get next vertex
    (setq vertex-ent (entnext vertex-ent))
  )
  
  T ; Return success
)

;; ============================================================================
;; OBJECT PROCESSOR - ARC
;; ============================================================================
;; Rounds the center point (10) of an arc
;; ============================================================================

(defun round-arc (ent entdata base-pt round-val / center-pt new-center)
  (setq center-pt (cdr (assoc 10 entdata)))
  (setq new-center (round-point center-pt base-pt round-val))
  
  ;; Update entity data
  (setq entdata (subst (cons 10 new-center) (assoc 10 entdata) entdata))
  
  ;; Apply changes to drawing
  (entmod entdata)
  (entupd ent)
  T ; Return success
)

;; ============================================================================
;; OBJECT PROCESSOR - SPLINE
;; ============================================================================
;; Rounds all control points (10) and fit points (11) of a spline
;; ============================================================================

(defun round-spline (ent entdata base-pt round-val / pt new-pt new-entdata)
  (setq new-entdata entdata)
  
  ;; Process control points (DXF code 10) and fit points (DXF code 11)
  (foreach item entdata
    (if (or (= (car item) 10) (= (car item) 11))
      (progn
        (setq pt (cdr item))
        (setq new-pt (round-point pt base-pt round-val))
        (setq new-entdata (subst (cons (car item) new-pt) item new-entdata))
      )
    )
  )
  
  ;; Apply changes to drawing
  (entmod new-entdata)
  (entupd ent)
  T ; Return success
)

;; ============================================================================
;; OBJECT PROCESSOR - INSERT (Block Reference)
;; ============================================================================
;; Rounds the insertion point (10) of a block reference
;; ============================================================================

(defun round-insert (ent entdata base-pt round-val / insert-pt new-insert)
  (setq insert-pt (cdr (assoc 10 entdata)))
  (setq new-insert (round-point insert-pt base-pt round-val))
  
  ;; Update entity data
  (setq entdata (subst (cons 10 new-insert) (assoc 10 entdata) entdata))
  
  ;; Apply changes to drawing
  (entmod entdata)
  (entupd ent)
  T ; Return success
)

;; ============================================================================
;; OBJECT PROCESSOR - TEXT
;; ============================================================================
;; Rounds the insertion point (10) and alignment point (11 if exists) of text
;; ============================================================================

(defun round-text (ent entdata base-pt round-val / insert-pt align-pt new-insert new-align)
  ;; Round insertion point (always present)
  (setq insert-pt (cdr (assoc 10 entdata)))
  (setq new-insert (round-point insert-pt base-pt round-val))
  (setq entdata (subst (cons 10 new-insert) (assoc 10 entdata) entdata))
  
  ;; Round alignment point if it exists (for justified text)
  (if (assoc 11 entdata)
    (progn
      (setq align-pt (cdr (assoc 11 entdata)))
      (setq new-align (round-point align-pt base-pt round-val))
      (setq entdata (subst (cons 11 new-align) (assoc 11 entdata) entdata))
    )
  )
  
  ;; Apply changes to drawing
  (entmod entdata)
  (entupd ent)
  T ; Return success
)

;; ============================================================================
;; OBJECT PROCESSOR - MTEXT
;; ============================================================================
;; Rounds the insertion point (10) of multiline text
;; ============================================================================

(defun round-mtext (ent entdata base-pt round-val / insert-pt new-insert)
  (setq insert-pt (cdr (assoc 10 entdata)))
  (setq new-insert (round-point insert-pt base-pt round-val))
  
  ;; Update entity data
  (setq entdata (subst (cons 10 new-insert) (assoc 10 entdata) entdata))
  
  ;; Apply changes to drawing
  (entmod entdata)
  (entupd ent)
  T ; Return success
)

;; ============================================================================
;; OBJECT PROCESSOR - DIMENSION
;; ============================================================================
;; Rounds dimension definition points (10, 13, 14, 15, 16)
;; DXF codes: 10=text position, 13=def point1, 14=def point2, 
;;            15=def point3, 16=def point4
;; ============================================================================

(defun round-dimension (ent entdata base-pt round-val / pt new-pt codes code)
  ;; List of DXF codes that contain points in dimensions
  (setq codes '(10 13 14 15 16))
  
  ;; Process each point code
  (foreach code codes
    (if (assoc code entdata)
      (progn
        (setq pt (cdr (assoc code entdata)))
        (setq new-pt (round-point pt base-pt round-val))
        (setq entdata (subst (cons code new-pt) (assoc code entdata) entdata))
      )
    )
  )
  
  ;; Apply changes to drawing
  (entmod entdata)
  (entupd ent)
  T ; Return success
)

;; ============================================================================
;; OBJECT PROCESSOR - LEADER
;; ============================================================================
;; Rounds all vertices (10) of a leader line
;; ============================================================================

(defun round-leader (ent entdata base-pt round-val / pt new-pt new-entdata)
  (setq new-entdata entdata)
  
  ;; Process each vertex (DXF code 10)
  (foreach item entdata
    (if (= (car item) 10)
      (progn
        (setq pt (cdr item))
        (setq new-pt (round-point pt base-pt round-val))
        (setq new-entdata (subst (cons 10 new-pt) item new-entdata))
      )
    )
  )
  
  ;; Apply changes to drawing
  (entmod new-entdata)
  (entupd ent)
  T ; Return success
)

;; ============================================================================
;; OBJECT PROCESSOR - HATCH
;; ============================================================================
;; Rounds the seed point (10) of a hatch pattern
;; Note: Hatch boundary geometry is not modified to preserve pattern integrity
;; ============================================================================

(defun round-hatch (ent entdata base-pt round-val / seed-pt new-seed)
  ;; Round seed point (base point for pattern)
  (if (assoc 10 entdata)
    (progn
      (setq seed-pt (cdr (assoc 10 entdata)))
      (setq new-seed (round-point seed-pt base-pt round-val))
      (setq entdata (subst (cons 10 new-seed) (assoc 10 entdata) entdata))
      
      ;; Apply changes to drawing
      (entmod entdata)
      (entupd ent)
      T ; Return success
    )
    nil ; Return nil if no seed point found
  )
)

;; ============================================================================
;; COMMAND INITIALIZATION MESSAGE
;; ============================================================================

(princ "\nCOORDRND v1.1 loaded successfully.")
(princ "\nType COORDRND to start coordinate rounding.")
(princ "\n")
(princ)
