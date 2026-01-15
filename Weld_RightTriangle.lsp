;; ============================================================================
;; RIGHT TRIANGLE DRAWER
;; AutoCAD 2020 LISP Script
;; ============================================================================
;; This script draws a right triangle based on two user-specified points.
;; The right angle is at the first point, and the line between the two points
;; forms one leg of the triangle.
;; 
;; Features:
;; - Dynamic preview that updates after each selection
;; - Optional arc on any side (first leg, second leg, or hypotenuse)
;; - Automatic solid hatch fill (sent to back)
;; - Uses current layer and ByLayer properties
;; ============================================================================

(defun C:WELDRT (/ pt1 pt2 pt3 dx dy side arc_side arc_side_str arc_radius arc_factor
                 leg1_length leg2_length hyp_length bulge preview_pline final_pline 
                 hatch_obj acad_obj doc_obj modelspace_obj pline_vla temp_factor)
  
  ;; Main function to draw a right triangle with optional arc on one side
  ;; Variables:
  ;;   pt1 - First point (right angle location)
  ;;   pt2 - Second point (end of one leg)
  ;;   pt3 - Third point (calculated for right angle)
  ;;   dx, dy - Delta X and Y between points
  ;;   side - User choice for triangle orientation (Left/Right, default is Left)
  ;;   arc_side - Which side should be an arc (0=none, 1=first leg, 2=second leg, 3=hypotenuse)
  ;;   arc_radius - Radius for the arc
  ;;   arc_factor - Multiplier for arc radius calculation (default 5.0)
  ;;   leg1_length, leg2_length, hyp_length - Triangle side lengths
  ;;   bulge - Bulge value for arc in polyline
  ;;   preview_pline - Temporary preview polyline
  ;;   final_pline - Final triangle polyline
  ;;   hatch_obj - Hatch entity for solid fill
  
  (princ "\n=== Right Triangle Drawer ===")
  
  ;; -------------------------------------------------------------------------
  ;; STEP 1: Get first point from user (this will be the right angle vertex)
  ;; -------------------------------------------------------------------------
  (setq pt1 (getpoint "\nSpecify first point (right angle vertex): "))
  
  ;; Check if user provided a valid point or cancelled
  (if pt1
    (progn
      
      ;; -----------------------------------------------------------------------
      ;; STEP 2: Get second point from user (end of one triangle leg)
      ;; -----------------------------------------------------------------------
      (setq pt2 (getpoint pt1 "\nSpecify second point: "))
      
      ;; Check if second point is valid
      (if pt2
        (progn
          
          ;; -------------------------------------------------------------------
          ;; STEP 3: Calculate the third point for the right triangle
          ;; -------------------------------------------------------------------
          ;; Calculate delta X and Y between the two points
          (setq dx (- (car pt2) (car pt1)))      ; Delta X
          (setq dy (- (cadr pt2) (cadr pt1)))    ; Delta Y
          
          ;; Calculate third point for right angle at pt1
          ;; Rotate 90 degrees counterclockwise (left-hand rule)
          (setq pt3 (list (- (car pt1) dy)
                          (+ (cadr pt1) dx)
                          (caddr pt1)))
          
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
          ;; STEP 5: Ask user which side to draw the triangle
          ;; -------------------------------------------------------------------
          (initget "Left Right")  ; Set up valid keyword responses (swapped order)
          (setq side (getkword "\nDraw triangle on [Left/Right] side <Left>: "))
          
          ;; If user pressed Enter or didn't specify, use default "Left"
          (if (not side)
            (setq side "Left"))
          
          ;; -------------------------------------------------------------------
          ;; STEP 6: Delete old preview and recalculate pt3 based on user's choice
          ;; -------------------------------------------------------------------
          (if preview_pline
            (entdel preview_pline))
          
          (if (= side "Right")
            (progn
              ;; Right side: rotate 90 degrees clockwise
              (setq pt3 (list (+ (car pt1) dy)
                              (- (cadr pt1) dx)
                              (caddr pt1)))
              (princ "\nUpdating preview - RIGHT side...")
            )
            (progn
              ;; Left side already calculated above
              (princ "\nUpdating preview - LEFT side...")
            )
          )
          
          ;; Draw updated preview with correct orientation
          (command "_.PLINE" pt1 pt2 pt3 "C")
          (setq preview_pline (entlast))
          
          ;; -------------------------------------------------------------------
          ;; STEP 5A: Ask user which side should be an arc
          ;; -------------------------------------------------------------------
          ;; Use word keywords instead of numbers for better UI interaction
          ;; This allows arrow key navigation and mouse selection in the prompt
          (initget "None First Second Hypotenuse")
          (setq arc_side_str (getkword "\nMake arc on which side? [None/First leg/Second leg/Hypotenuse] <Second leg>: "))
          
          ;; Convert keyword to numeric value, default to 2 (Second leg) if not specified
          (cond
            ((= arc_side_str "None") (setq arc_side 0))
            ((= arc_side_str "First") (setq arc_side 1))
            ((= arc_side_str "Hypotenuse") (setq arc_side 3))
            (t (setq arc_side 2))  ; Default: Second leg (includes nil case)
          )
          
          ;; -------------------------------------------------------------------
          ;; STEP 5A.1: Update preview immediately to show arc location
          ;; -------------------------------------------------------------------
          (if (/= arc_side 0)
            (progn
              ;; Calculate side lengths for preview arc
              (setq leg1_length (distance pt1 pt2))
              (setq leg2_length (distance pt1 pt3))
              (setq hyp_length (distance pt2 pt3))
              
              ;; Use default arc factor for preview
              (setq arc_factor 5.0)
              
              ;; Calculate bulge for preview with default factor
              (cond
                ((= arc_side 1)
                  (setq arc_radius (* arc_factor leg1_length))
                  (setq bulge (/ leg1_length (* 2.0 arc_radius)))
                  (if (= side "Right") (setq bulge (- bulge)))
                )
                ((= arc_side 2)
                  (setq arc_radius (* arc_factor leg2_length))
                  (setq bulge (/ leg2_length (* 2.0 arc_radius)))
                  (if (= side "Right") (setq bulge (- bulge)))
                )
                ((= arc_side 3)
                  (setq arc_radius (* arc_factor hyp_length))
                  (setq bulge (/ hyp_length (* 2.0 arc_radius)))
                  (if (= side "Right") (setq bulge (- bulge)))
                )
              )
              
              ;; Delete old preview
              (if preview_pline (entdel preview_pline))
              
              ;; Draw preview with arc using default factor
              (princ "\nUpdating preview with arc...")
              (cond
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
              )
              
              ;; Save preview polyline entity
              (setq preview_pline (entlast))
            )
          )
          
          ;; -------------------------------------------------------------------
          ;; STEP 7: If arc was selected, ask for custom arc factor
          ;; -------------------------------------------------------------------
          (if (/= arc_side 0)
            (progn
              (princ (strcat "\n[Preview shows arc with factor: " (rtos arc_factor 2 1) "]"))
              (setq temp_factor (getreal "\nEnter arc factor (suggested 1.5-5.0) <5.0>: "))
              
              ;; If user entered a value, use it; otherwise keep default
              (if temp_factor
                (setq arc_factor temp_factor))
              
              ;; -------------------------------------------------------------------
              ;; STEP 7A: UPDATE PREVIEW WITH NEW ARC FACTOR (FIX)
              ;; -------------------------------------------------------------------
              ;; Recalculate bulge with user's chosen arc factor
              (cond
                ((= arc_side 1)
                  (setq arc_radius (* arc_factor leg1_length))
                  (setq bulge (/ leg1_length (* 2.0 arc_radius)))
                  (if (= side "Right") (setq bulge (- bulge)))
                )
                ((= arc_side 2)
                  (setq arc_radius (* arc_factor leg2_length))
                  (setq bulge (/ leg2_length (* 2.0 arc_radius)))
                  (if (= side "Right") (setq bulge (- bulge)))
                )
                ((= arc_side 3)
                  (setq arc_radius (* arc_factor hyp_length))
                  (setq bulge (/ hyp_length (* 2.0 arc_radius)))
                  (if (= side "Right") (setq bulge (- bulge)))
                )
              )
              
              ;; Delete old preview
              (if preview_pline (entdel preview_pline))
              
              ;; Draw updated preview with new arc factor
              (princ "\nUpdating preview with new arc factor...")
              (cond
                ((= arc_side 1)
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
                ((= arc_side 2)
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
                ((= arc_side 3)
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
          ;; STEP 8: Calculate triangle properties for final drawing
          ;; -------------------------------------------------------------------
          (if (not leg1_length)  ; Only calculate if not already done
            (progn
              (setq leg1_length (distance pt1 pt2))
              (setq leg2_length (distance pt1 pt3))
              (setq hyp_length (distance pt2 pt3))
            )
          )
          
          ;; If arc was chosen, calculate final radius and bulge if not already done
          (if (and (/= arc_side 0) (not arc_radius))
            (progn
              (cond
                ((= arc_side 1)
                  (setq arc_radius (* arc_factor leg1_length))
                  (setq bulge (/ leg1_length (* 2.0 arc_radius)))
                  (if (= side "Right") (setq bulge (- bulge)))
                )
                ((= arc_side 2)
                  (setq arc_radius (* arc_factor leg2_length))
                  (setq bulge (/ leg2_length (* 2.0 arc_radius)))
                  (if (= side "Right") (setq bulge (- bulge)))
                )
                ((= arc_side 3)
                  (setq arc_radius (* arc_factor hyp_length))
                  (setq bulge (/ hyp_length (* 2.0 arc_radius)))
                  (if (= side "Right") (setq bulge (- bulge)))
                )
              )
            )
          )
          
          ;; -------------------------------------------------------------------
          ;; STEP 8A: Delete preview before creating final triangle
          ;; -------------------------------------------------------------------
          (if preview_pline
            (entdel preview_pline))
          
          (princ "\nCreating final triangle...")
          
          ;; -------------------------------------------------------------------
          ;; STEP 8B: Create the final polyline with arc (if specified)
          ;; -------------------------------------------------------------------
          (cond
            ;; Arc on first leg (bulge at pt1, affects segment from pt1 to pt2)
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
            
            ;; Arc on second leg (bulge at pt3, affects segment from pt3 to pt1)
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
            
            ;; Arc on hypotenuse (bulge at pt2, affects segment from pt2 to pt3)
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
          ;; STEP 9: Create solid hatch inside the triangle using VLA methods
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
          ;; STEP 9A: Send hatch to back
          ;; -------------------------------------------------------------------
          (command "_.DRAWORDER" hatch_obj "" "_B")
          
          ;; -------------------------------------------------------------------
          ;; STEP 10: Verify hatch was created successfully
          ;; -------------------------------------------------------------------
          (if hatch_obj
            (progn
              (princ "\nRight triangle created successfully!")
              (princ "\nTriangle properties:")
              (princ (strcat "\n  - First point (right angle): " 
                           (rtos (car pt1) 2 2) ", " 
                           (rtos (cadr pt1) 2 2)))
              (princ (strcat "\n  - Second point: " 
                           (rtos (car pt2) 2 2) ", " 
                           (rtos (cadr pt2) 2 2)))
              (princ (strcat "\n  - Third point: " 
                           (rtos (car pt3) 2 2) ", " 
                           (rtos (cadr pt3) 2 2)))
              (princ (strcat "\n  - Orientation: " side " side"))
              (princ (strcat "\n  - First leg: " (rtos leg1_length 2 2)))
              (princ (strcat "\n  - Second leg: " (rtos leg2_length 2 2)))
              (princ (strcat "\n  - Hypotenuse: " (rtos hyp_length 2 2)))
              (if (/= arc_side 0)
                (progn
                  (princ (strcat "\n  - Arc on: " 
                    (cond 
                      ((= arc_side 1) "First leg")
                      ((= arc_side 2) "Second leg")
                      ((= arc_side 3) "Hypotenuse")
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
;; The command is registered as WELDRT (Weld Right Triangle)
;; User can type WELDRT at the AutoCAD command line to execute

(princ "\n=== Right Triangle Script Loaded ===")
(princ "\nType WELDRT to draw a right triangle with optional arc")
(princ "\n")
(princ "\nFeatures:")
(princ "\n  - Dynamic preview updates after each selection")
(princ "\n  - Arrow keys or mouse to select options")
(princ "\n  - Arc on any side: None, First leg, Second leg (default), Hypotenuse")
(princ "\n  - Preview updates when you enter custom arc factor")
(princ "\n")
(princ "\nArc radius = factor * leg_length (default factor: 5.0)")
(princ "\nSuggested: 5.0 (gentle), 3.0 (moderate), 1.5 (sharp)")
(princ "\n")
(princ)

;; ============================================================================
;; END OF SCRIPT
;; ============================================================================
