;; ============================================================================
;; 3-POINT TRIANGLE DRAWER
;; AutoCAD 2020 LISP Script
;; ============================================================================
;; This script draws a triangle based on three user-specified points.
;; The first point is the apex/center of angle, and the other two points
;; define the ends of the two sides.
;; 
;; Features:
;; - Dynamic preview that updates after each selection
;; - Optional arc on any side (side 12, side 13, or side 23)
;; - Default arc on side 23 (between points 2 and 3)
;; - Automatic solid hatch fill (sent to back)
;; - Uses current layer and ByLayer properties
;; ============================================================================

(defun C:WELD3T (/ pt1 pt2 pt3 arc_side arc_side_str arc_radius arc_factor
                   side12_length side13_length side23_length bulge preview_pline 
                   final_pline hatch_obj acad_obj doc_obj modelspace_obj pline_vla temp_factor
                   bulge_sign)
  
  ;; Main function to draw a 3-point triangle with optional arc on one side
  ;; Variables:
  ;;   pt1 - First point (apex/center of angle)
  ;;   pt2 - Second point (end of first side)
  ;;   pt3 - Third point (end of second side)
  ;;   arc_side - Which side should be an arc (0=none, 1=side 12, 2=side 13, 3=side 23)
  ;;   arc_radius - Radius for the arc
  ;;   arc_factor - Multiplier for arc radius calculation (default 5.0)
  ;;   side12_length, side13_length, side23_length - Triangle side lengths
  ;;   bulge - Bulge value for arc in polyline
  ;;   preview_pline - Temporary preview polyline
  ;;   final_pline - Final triangle polyline
  ;;   hatch_obj - Hatch entity for solid fill
  
  (princ "\n=== 3-Point Triangle Drawer ===")
  
  ;; -------------------------------------------------------------------------
  ;; STEP 1: Get first point from user (apex/center of angle)
  ;; -------------------------------------------------------------------------
  (setq pt1 (getpoint "\nSpecify first point (apex/center of angle): "))
  
  ;; Check if user provided a valid point or cancelled
  (if pt1
    (progn
      
      ;; -----------------------------------------------------------------------
      ;; STEP 2: Get second point from user (end of first side)
      ;; -----------------------------------------------------------------------
      (setq pt2 (getpoint pt1 "\nSpecify second point (end of first side): "))
      
      ;; Check if second point is valid
      (if pt2
        (progn
          
          ;; -------------------------------------------------------------------
          ;; STEP 3: Get third point from user (end of second side)
          ;; -------------------------------------------------------------------
          (setq pt3 (getpoint pt1 "\nSpecify third point (end of second side): "))
          
          ;; Check if third point is valid
          (if pt3
            (progn
              
              ;; -------------------------------------------------------------------
              ;; STEP 4: Draw preview triangle
              ;; -------------------------------------------------------------------
              (princ "\nDrawing preview...")
              
              ;; Create closed polyline: pt1 -> pt2 -> pt3 -> pt1
              (command "_.PLINE" 
                       pt1
                       pt2
                       pt3
                       "C")
              
              (setq preview_pline (entlast))
              
              ;; -------------------------------------------------------------------
              ;; STEP 5: Ask user which side should be an arc
              ;; -------------------------------------------------------------------
              ;; Use word keywords for better UI interaction
              (initget "None Side12 Side13 Side23")
              (setq arc_side_str (getkword "\nMake arc on which side? [None/Side12/Side13/Side23] <Side23>: "))
              
              ;; Convert keyword to numeric value, default to 3 (Side 23) if not specified
              (cond
                ((= arc_side_str "None") (setq arc_side 0))
                ((= arc_side_str "Side12") (setq arc_side 1))
                ((= arc_side_str "Side13") (setq arc_side 2))
                (t (setq arc_side 3))  ; Default: Side 23 (includes nil case)
              )
              
              ;; -------------------------------------------------------------------
              ;; STEP 5A: Update preview immediately to show arc location
              ;; -------------------------------------------------------------------
              (if (/= arc_side 0)
                (progn
                  ;; Calculate side lengths for preview arc
                  (setq side12_length (distance pt1 pt2))
                  (setq side13_length (distance pt1 pt3))
                  (setq side23_length (distance pt2 pt3))
                  
                  ;; Use default arc factor for preview
                  (setq arc_factor 5.0)
                  
                  ;; Calculate bulge for preview with default factor
                  ;; Need to determine if arc should bulge inward or outward
                  ;; Use cross product to determine orientation
                  (setq bulge_sign 
                    (if (> (- (* (- (car pt2) (car pt1)) (- (cadr pt3) (cadr pt1)))
                              (* (- (cadr pt2) (cadr pt1)) (- (car pt3) (car pt1)))) 0)
                      1.0
                      -1.0))
                  
                  (cond
                    ((= arc_side 1)  ; Arc on side 12 (pt1 to pt2)
                      (setq arc_radius (* arc_factor side12_length))
                      (setq bulge (* bulge_sign (/ side12_length (* 2.0 arc_radius))))
                    )
                    ((= arc_side 2)  ; Arc on side 13 (pt1 to pt3)
                      (setq arc_radius (* arc_factor side13_length))
                      (setq bulge (* bulge_sign (/ side13_length (* 2.0 arc_radius))))
                    )
                    ((= arc_side 3)  ; Arc on side 23 (pt2 to pt3)
                      (setq arc_radius (* arc_factor side23_length))
                      (setq bulge (* bulge_sign (/ side23_length (* 2.0 arc_radius))))
                    )
                  )
                  
                  ;; Delete old preview
                  (if preview_pline (entdel preview_pline))
                  
                  ;; Draw preview with arc using default factor
                  (princ "\nUpdating preview with arc...")
                  (cond
                    ((= arc_side 1)  ; Arc on side 12 (bulge at pt1)
                      (entmake (list '(0 . "LWPOLYLINE")
                                   '(100 . "AcDbEntity")
                                   '(100 . "AcDbPolyline")
                                   '(90 . 3)                    ; 3 vertices
                                   '(70 . 1)                    ; Closed flag
                                   (cons 10 (list (car pt1) (cadr pt1)))   ; Vertex 1
                                   (cons 42 bulge)              ; Bulge for arc to next vertex
                                   (cons 10 (list (car pt2) (cadr pt2)))   ; Vertex 2
                                   (cons 42 0.0)                ; No bulge (straight line)
                                   (cons 10 (list (car pt3) (cadr pt3)))   ; Vertex 3
                                   (cons 42 0.0)                ; No bulge (straight line)
                      ))
                    )
                    ((= arc_side 2)  ; Arc on side 13 (bulge at pt3)
                      (entmake (list '(0 . "LWPOLYLINE")
                                   '(100 . "AcDbEntity")
                                   '(100 . "AcDbPolyline")
                                   '(90 . 3)                    ; 3 vertices
                                   '(70 . 1)                    ; Closed flag
                                   (cons 10 (list (car pt1) (cadr pt1)))   ; Vertex 1
                                   (cons 42 0.0)                ; No bulge (straight line)
                                   (cons 10 (list (car pt2) (cadr pt2)))   ; Vertex 2
                                   (cons 42 0.0)                ; No bulge (straight line)
                                   (cons 10 (list (car pt3) (cadr pt3)))   ; Vertex 3
                                   (cons 42 bulge)              ; Bulge for arc back to pt1
                      ))
                    )
                    ((= arc_side 3)  ; Arc on side 23 (bulge at pt2)
                      (entmake (list '(0 . "LWPOLYLINE")
                                   '(100 . "AcDbEntity")
                                   '(100 . "AcDbPolyline")
                                   '(90 . 3)                    ; 3 vertices
                                   '(70 . 1)                    ; Closed flag
                                   (cons 10 (list (car pt1) (cadr pt1)))   ; Vertex 1
                                   (cons 42 0.0)                ; No bulge (straight line)
                                   (cons 10 (list (car pt2) (cadr pt2)))   ; Vertex 2
                                   (cons 42 bulge)              ; Bulge for arc to next vertex
                                   (cons 10 (list (car pt3) (cadr pt3)))   ; Vertex 3
                                   (cons 42 0.0)                ; No bulge (straight line)
                      ))
                    )
                  )
                  
                  ;; Save preview polyline entity
                  (setq preview_pline (entlast))
                )
              )
              
              ;; -------------------------------------------------------------------
              ;; STEP 6: If arc was selected, ask for custom arc factor
              ;; -------------------------------------------------------------------
              (if (/= arc_side 0)
                (progn
                  (princ (strcat "\n[Preview shows arc with factor: " (rtos arc_factor 2 1) "]"))
                  (setq temp_factor (getreal "\nEnter arc factor (suggested 1.5-5.0) <5.0>: "))
                  
                  ;; If user entered a value, use it; otherwise keep default
                  (if temp_factor
                    (setq arc_factor temp_factor))
                  
                  ;; -------------------------------------------------------------------
                  ;; STEP 6A: UPDATE PREVIEW WITH NEW ARC FACTOR
                  ;; -------------------------------------------------------------------
                  ;; Recalculate bulge with user's chosen arc factor
                  (cond
                    ((= arc_side 1)  ; Arc on side 12
                      (setq arc_radius (* arc_factor side12_length))
                      (setq bulge (* bulge_sign (/ side12_length (* 2.0 arc_radius))))
                    )
                    ((= arc_side 2)  ; Arc on side 13
                      (setq arc_radius (* arc_factor side13_length))
                      (setq bulge (* bulge_sign (/ side13_length (* 2.0 arc_radius))))
                    )
                    ((= arc_side 3)  ; Arc on side 23
                      (setq arc_radius (* arc_factor side23_length))
                      (setq bulge (* bulge_sign (/ side23_length (* 2.0 arc_radius))))
                    )
                  )
                  
                  ;; Delete old preview
                  (if preview_pline (entdel preview_pline))
                  
                  ;; Draw updated preview with new arc factor
                  (princ "\nUpdating preview with new arc factor...")
                  (cond
                    ((= arc_side 1)  ; Arc on side 12
                      (entmake (list '(0 . "LWPOLYLINE")
                                   '(100 . "AcDbEntity")
                                   '(100 . "AcDbPolyline")
                                   '(90 . 3)
                                   '(70 . 1)
                                   (cons 10 (list (car pt1) (cadr pt1)))
                                   (cons 42 bulge)
                                   (cons 10 (list (car pt2) (cadr pt2)))
                                   (cons 42 0.0)
                                   (cons 10 (list (car pt3) (cadr pt3)))
                                   (cons 42 0.0)
                      ))
                    )
                    ((= arc_side 2)  ; Arc on side 13
                      (entmake (list '(0 . "LWPOLYLINE")
                                   '(100 . "AcDbEntity")
                                   '(100 . "AcDbPolyline")
                                   '(90 . 3)
                                   '(70 . 1)
                                   (cons 10 (list (car pt1) (cadr pt1)))
                                   (cons 42 0.0)
                                   (cons 10 (list (car pt2) (cadr pt2)))
                                   (cons 42 0.0)
                                   (cons 10 (list (car pt3) (cadr pt3)))
                                   (cons 42 bulge)
                      ))
                    )
                    ((= arc_side 3)  ; Arc on side 23
                      (entmake (list '(0 . "LWPOLYLINE")
                                   '(100 . "AcDbEntity")
                                   '(100 . "AcDbPolyline")
                                   '(90 . 3)
                                   '(70 . 1)
                                   (cons 10 (list (car pt1) (cadr pt1)))
                                   (cons 42 0.0)
                                   (cons 10 (list (car pt2) (cadr pt2)))
                                   (cons 42 bulge)
                                   (cons 10 (list (car pt3) (cadr pt3)))
                                   (cons 42 0.0)
                      ))
                    )
                  )
                  
                  ;; Save updated preview
                  (setq preview_pline (entlast))
                )
              )
              
              ;; -------------------------------------------------------------------
              ;; STEP 7: Calculate triangle properties for final drawing
              ;; -------------------------------------------------------------------
              (if (not side12_length)  ; Only calculate if not already done
                (progn
                  (setq side12_length (distance pt1 pt2))
                  (setq side13_length (distance pt1 pt3))
                  (setq side23_length (distance pt2 pt3))
                )
              )
              
              ;; If arc was chosen, calculate final radius and bulge if not already done
              (if (and (/= arc_side 0) (not arc_radius))
                (progn
                  ;; Calculate bulge sign
                  (setq bulge_sign 
                    (if (> (- (* (- (car pt2) (car pt1)) (- (cadr pt3) (cadr pt1)))
                              (* (- (cadr pt2) (cadr pt1)) (- (car pt3) (car pt1)))) 0)
                      1.0
                      -1.0))
                  
                  (cond
                    ((= arc_side 1)
                      (setq arc_radius (* arc_factor side12_length))
                      (setq bulge (* bulge_sign (/ side12_length (* 2.0 arc_radius))))
                    )
                    ((= arc_side 2)
                      (setq arc_radius (* arc_factor side13_length))
                      (setq bulge (* bulge_sign (/ side13_length (* 2.0 arc_radius))))
                    )
                    ((= arc_side 3)
                      (setq arc_radius (* arc_factor side23_length))
                      (setq bulge (* bulge_sign (/ side23_length (* 2.0 arc_radius))))
                    )
                  )
                )
              )
              
              ;; -------------------------------------------------------------------
              ;; STEP 7A: Delete preview before creating final triangle
              ;; -------------------------------------------------------------------
              (if preview_pline
                (entdel preview_pline))
              
              (princ "\nCreating final triangle...")
              
              ;; -------------------------------------------------------------------
              ;; STEP 7B: Create the final polyline with arc (if specified)
              ;; -------------------------------------------------------------------
              (cond
                ;; Arc on side 12 (bulge at pt1, affects segment from pt1 to pt2)
                ((= arc_side 1)
                  (entmake (list '(0 . "LWPOLYLINE")
                               '(100 . "AcDbEntity")
                               '(100 . "AcDbPolyline")
                               '(90 . 3)                    ; 3 vertices
                               '(70 . 1)                    ; Closed flag
                               (cons 10 (list (car pt1) (cadr pt1)))   ; Vertex 1
                               (cons 42 bulge)              ; Bulge for arc to next vertex
                               (cons 10 (list (car pt2) (cadr pt2)))   ; Vertex 2
                               (cons 42 0.0)                ; No bulge (straight line)
                               (cons 10 (list (car pt3) (cadr pt3)))   ; Vertex 3
                               (cons 42 0.0)                ; No bulge (straight line)
                  ))
                )
                
                ;; Arc on side 13 (bulge at pt3, affects segment from pt3 to pt1)
                ((= arc_side 2)
                  (entmake (list '(0 . "LWPOLYLINE")
                               '(100 . "AcDbEntity")
                               '(100 . "AcDbPolyline")
                               '(90 . 3)                    ; 3 vertices
                               '(70 . 1)                    ; Closed flag
                               (cons 10 (list (car pt1) (cadr pt1)))   ; Vertex 1
                               (cons 42 0.0)                ; No bulge (straight line)
                               (cons 10 (list (car pt2) (cadr pt2)))   ; Vertex 2
                               (cons 42 0.0)                ; No bulge (straight line)
                               (cons 10 (list (car pt3) (cadr pt3)))   ; Vertex 3
                               (cons 42 bulge)              ; Bulge for arc back to pt1
                  ))
                )
                
                ;; Arc on side 23 (bulge at pt2, affects segment from pt2 to pt3)
                ((= arc_side 3)
                  (entmake (list '(0 . "LWPOLYLINE")
                               '(100 . "AcDbEntity")
                               '(100 . "AcDbPolyline")
                               '(90 . 3)                    ; 3 vertices
                               '(70 . 1)                    ; Closed flag
                               (cons 10 (list (car pt1) (cadr pt1)))   ; Vertex 1
                               (cons 42 0.0)                ; No bulge (straight line)
                               (cons 10 (list (car pt2) (cadr pt2)))   ; Vertex 2
                               (cons 42 bulge)              ; Bulge for arc to next vertex
                               (cons 10 (list (car pt3) (cadr pt3)))   ; Vertex 3
                               (cons 42 0.0)                ; No bulge (straight line)
                  ))
                )
                
                ;; No arc - standard straight-sided triangle
                (t
                  (command "_.PLINE" 
                           pt1           ; Start at first point
                           pt2           ; Draw to second point
                           pt3           ; Draw to third point
                           "C")          ; Close back to start
                )
              )
              
              ;; Get the entity name of the final triangle polyline
              (setq final_pline (entlast))
              
              ;; -------------------------------------------------------------------
              ;; STEP 8: Create solid hatch inside the triangle using VLA methods
              ;; -------------------------------------------------------------------
              (princ "\nApplying solid hatch...")
              
              ;; Force command completion to ensure polyline is fully created
              (command)
              
              ;; Load Visual LISP extensions if not already loaded
              (vl-load-com)
              
              ;; Get the ActiveX document object
              (setq acad_obj (vlax-get-acad-object))
              (setq doc_obj (vla-get-ActiveDocument acad_obj))
              (setq modelspace_obj (vla-get-ModelSpace doc_obj))
              
              ;; Convert the polyline entity name to VLA object
              (setq pline_vla (vlax-ename->vla-object final_pline))
              
              ;; Create the hatch using AddHatch method
              (setq hatch_obj 
                (vla-AddHatch modelspace_obj 
                              0           ; Normal hatch
                              "SOLID"     ; Solid fill pattern
                              :vlax-true  ; Associative
                )
              )
              
              ;; Set the polyline as the hatch boundary
              (vlax-invoke hatch_obj 'AppendOuterLoop (list pline_vla))
              
              ;; Generate the hatch fill
              (vla-Evaluate hatch_obj)
              
              ;; Use ByLayer properties
              (vla-put-Color hatch_obj acByLayer)
              
              ;; Convert back to entity name
              (setq hatch_obj (vlax-vla-object->ename hatch_obj))
              
              ;; -------------------------------------------------------------------
              ;; STEP 8A: Send hatch to back
              ;; -------------------------------------------------------------------
              (command "_.DRAWORDER" hatch_obj "" "_B")
              
              ;; -------------------------------------------------------------------
              ;; STEP 9: Verify hatch was created successfully
              ;; -------------------------------------------------------------------
              (if hatch_obj
                (progn
                  (princ "\n3-point triangle created successfully!")
                  (princ "\nTriangle properties:")
                  (princ (strcat "\n  - Point 1 (apex): " 
                               (rtos (car pt1) 2 2) ", " 
                               (rtos (cadr pt1) 2 2)))
                  (princ (strcat "\n  - Point 2: " 
                               (rtos (car pt2) 2 2) ", " 
                               (rtos (cadr pt2) 2 2)))
                  (princ (strcat "\n  - Point 3: " 
                               (rtos (car pt3) 2 2) ", " 
                               (rtos (cadr pt3) 2 2)))
                  (princ (strcat "\n  - Side 12 length: " (rtos side12_length 2 2)))
                  (princ (strcat "\n  - Side 13 length: " (rtos side13_length 2 2)))
                  (princ (strcat "\n  - Side 23 length: " (rtos side23_length 2 2)))
                  (if (/= arc_side 0)
                    (progn
                      (princ (strcat "\n  - Arc on: " 
                        (cond 
                          ((= arc_side 1) "Side 12 (pt1 to pt2)")
                          ((= arc_side 2) "Side 13 (pt1 to pt3)")
                          ((= arc_side 3) "Side 23 (pt2 to pt3)")
                        )
                      ))
                      (princ (strcat "\n  - Arc radius: " (rtos arc_radius 2 2)))
                      (princ (strcat "\n  - Arc factor: " (rtos arc_factor 2 1)))
                    )
                    (princ "\n  - Straight sides (no arc)")
                  )
                )
                ;; If hatch failed, inform user
                (princ "\nWarning: Hatch may not have been created properly.")
              )
            )
            ;; If third point was not provided (user cancelled)
            (princ "\nCommand cancelled - third point not specified.")
          )
        )
        ;; If second point was not provided (user cancelled)
        (princ "\nCommand cancelled - second point not specified.")
      )
    )
    ;; If first point was not provided (user cancelled)
    (princ "\nCommand cancelled - first point not specified.")
  )
  
  ;; End the command cleanly
  (princ)
)

;; ============================================================================
;; COMMAND REGISTRATION
;; ============================================================================
;; The command is registered as WELD3T (Weld 3-point Triangle)
;; User can type WELD3T at the AutoCAD command line to execute

(princ "\n=== 3-Point Triangle Script Loaded ===")
(princ "\nType WELD3T to draw a 3-point triangle with optional arc")
(princ "\n")
(princ "\nFeatures:")
(princ "\n  - Define all 3 points of the triangle")
(princ "\n  - Dynamic preview updates after each selection")
(princ "\n  - Arc on any side: None, Side 12, Side 13, Side 23 (default)")
(princ "\n  - Preview updates when you enter custom arc factor")
(princ "\n")
(princ "\nArc radius = factor * side_length (default factor: 5.0)")
(princ "\nSuggested: 5.0 (gentle), 3.0 (moderate), 1.5 (sharp)")
(princ "\n")
(princ)

;; ============================================================================
;; END OF SCRIPT
;; ============================================================================
