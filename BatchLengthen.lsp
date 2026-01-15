;;; ============================================================================
;;; BATCHLENGTHEN.LSP
;;; 
;;; Purpose: Lengthen or shorten both ends of selected LINE, PLINE, and 3DPOLY objects
;;; Compatible with: AutoCAD 2020 and later
;;; 
;;; Usage: Type BATCHLENGTHEN at the command line
;;;        - Select objects (LINE, PLINE, 3DPOLY)
;;;        - Enter length value (positive to lengthen, negative to shorten)
;;;        - Both ends of each object will be modified
;;; ============================================================================

(defun C:BATCHLENGTHEN (/ ss length-value i ent obj obj-type result)
  
  ;; Function: Display welcome message and instructions
  (princ "\n=== BATCH LENGTHEN BOTH ENDS ===")
  (princ "\nSelect LINE, PLINE, or 3DPOLY objects to modify.")
  
  ;; Prompt user to select objects
  ;; Filter selection to only LINE, POLYLINE, and 3DPOLY entity types
  (setq ss (ssget (list 
                    (cons 0 "LINE,LWPOLYLINE,POLYLINE")
                  )))
  
  ;; Check if user made a valid selection
  (if ss
    (progn
      ;; Display number of selected objects
      (princ (strcat "\n" (itoa (sslength ss)) " object(s) selected."))
      
      ;; Prompt user for length value with default of -300
      ;; Positive value = lengthen, Negative value = shorten
      ;; Note: No initget restriction - we allow both positive and negative values
      (setq length-value (getreal "\nEnter length to add/subtract (positive to lengthen, negative to shorten) <-300>: "))
      
      ;; If user pressed Enter without input, use default value
      (if (not length-value)
        (setq length-value -300)
      )
      
      ;; Check if user entered a valid length value
      (if length-value
        (progn
          ;; Start undo mark for entire operation (using proper language-independent syntax)
          (command "_.UNDO" "_Begin")
          
          ;; Initialize counter for loop
          (setq i 0)
          
          ;; Loop through all selected objects
          (repeat (sslength ss)
            
            ;; Get entity name from selection set
            (setq ent (ssname ss i))
            
            ;; Get entity data list
            (setq obj (entget ent))
            
            ;; Get object type from DXF code 0
            (setq obj-type (cdr (assoc 0 obj)))
            
            ;; Process object based on its type
            (cond
              
              ;; ========== Process LINE objects ==========
              ((= obj-type "LINE")
                (setq result (lengthen-line ent length-value))
                (if result
                  (princ (strcat "\n✓ LINE lengthened successfully"))
                  (princ (strcat "\n✗ Failed to lengthen LINE"))
                )
              )
              
              ;; ========== Process LWPOLYLINE objects ==========
              ((= obj-type "LWPOLYLINE")
                (setq result (lengthen-lwpolyline ent length-value))
                (if result
                  (princ (strcat "\n✓ LWPOLYLINE lengthened successfully"))
                  (princ (strcat "\n✗ Failed to lengthen LWPOLYLINE"))
                )
              )
              
              ;; ========== Process POLYLINE objects (including 3DPOLY) ==========
              ((= obj-type "POLYLINE")
                (setq result (lengthen-polyline ent length-value))
                (if result
                  (princ (strcat "\n✓ POLYLINE/3DPOLY lengthened successfully"))
                  (princ (strcat "\n✗ Failed to lengthen POLYLINE/3DPOLY"))
                )
              )
              
            ) ;; end cond
            
            ;; Increment counter
            (setq i (1+ i))
            
          ) ;; end repeat
          
          ;; End undo mark
          (command "_.UNDO" "E")
          
          ;; Display completion message
          (princ "\n=== Operation Complete ===")
          
        ) ;; end progn - valid length value
        (princ "\n*** Operation cancelled - no length value entered. ***")
      ) ;; end if length-value
      
    ) ;; end progn - valid selection
    (princ "\n*** No objects selected or invalid selection. ***")
  ) ;; end if ss
  
  ;; Clean exit
  (princ)
)


;;; ============================================================================
;;; Function: LENGTHEN-LINE
;;; Purpose: Lengthen or shorten both ends of a LINE object
;;; Arguments: 
;;;   ent - Entity name of the LINE
;;;   len - Length to add (positive) or subtract (negative)
;;; Returns: T on success, nil on failure
;;; ============================================================================
(defun lengthen-line (ent len / obj start-pt end-pt dir-vec1 dir-vec2 mag new-start new-end)
  
  ;; Get entity data
  (setq obj (entget ent))
  
  ;; Extract start point (DXF code 10) and end point (DXF code 11)
  (setq start-pt (cdr (assoc 10 obj)))
  (setq end-pt (cdr (assoc 11 obj)))
  
  ;; Calculate magnitude (length) of the line
  (setq mag (distance start-pt end-pt))
  
  ;; Check if line has zero or near-zero length (degenerate line)
  (if (< mag 0.0001)
    (progn
      (princ "\n  Warning: Zero-length line detected, skipping...")
      nil  ; Return failure for zero-length line
    )
    (progn
      ;; Calculate direction vector from end to start (for extending start point outward)
      (setq dir-vec1 (list 
                       (- (car start-pt) (car end-pt))      ; X component
                       (- (cadr start-pt) (cadr end-pt))    ; Y component
                       (- (caddr start-pt) (caddr end-pt))  ; Z component
                     ))
      
      ;; Normalize direction vector (make it unit length)
      (setq dir-vec1 (list 
                       (/ (car dir-vec1) mag)
                       (/ (cadr dir-vec1) mag)
                       (/ (caddr dir-vec1) mag)
                     ))
      
      ;; Calculate direction vector from start to end (for extending end point outward)
      (setq dir-vec2 (list 
                       (- (car end-pt) (car start-pt))      ; X component
                       (- (cadr end-pt) (cadr start-pt))    ; Y component
                       (- (caddr end-pt) (caddr start-pt))  ; Z component
                     ))
      
      ;; Normalize end direction vector
      (setq dir-vec2 (list 
                       (/ (car dir-vec2) mag)
                       (/ (cadr dir-vec2) mag)
                       (/ (caddr dir-vec2) mag)
                     ))
      
      ;; Calculate new start point by adding length along direction vector
      (setq new-start (list 
                        (+ (car start-pt) (* (car dir-vec1) len))      ; New X
                        (+ (cadr start-pt) (* (cadr dir-vec1) len))    ; New Y
                        (+ (caddr start-pt) (* (caddr dir-vec1) len))  ; New Z
                      ))
      
      ;; Calculate new end point by adding length along direction vector
      (setq new-end (list 
                      (+ (car end-pt) (* (car dir-vec2) len))      ; New X
                      (+ (cadr end-pt) (* (cadr dir-vec2) len))    ; New Y
                      (+ (caddr end-pt) (* (caddr dir-vec2) len))  ; New Z
                    ))
      
      ;; Update entity data with new start point
      (setq obj (subst (cons 10 new-start) (assoc 10 obj) obj))
      
      ;; Update entity data with new end point
      (setq obj (subst (cons 11 new-end) (assoc 11 obj) obj))
      
      ;; Write modified entity data back to drawing database
      (entmod obj)
      
      ;; Update display
      (entupd ent)
      
      ;; Return success
      T
    )
  )
)




;;; ============================================================================
;;; Function: LENGTHEN-LWPOLYLINE
;;; Purpose: Lengthen or shorten both ends of a LWPOLYLINE object
;;; Arguments: 
;;;   ent - Entity name of the LWPOLYLINE
;;;   len - Length to add (positive) or subtract (negative)
;;; Returns: T on success, nil on failure
;;; ============================================================================
(defun lengthen-lwpolyline (ent len / obj vertex-list first-pt second-pt last-pt second-last-pt 
                             dir-vec1 dir-vec2 mag1 mag2 new-first new-last)
  
  ;; Get entity data
  (setq obj (entget ent))
  
  ;; Extract all vertices from the polyline
  ;; Vertices are stored as DXF code 10
  (setq vertex-list '())
  (foreach item obj
    (if (= (car item) 10)
      (setq vertex-list (append vertex-list (list (cdr item))))
    )
  )
  
  ;; Check if we have at least 2 vertices
  (if (< (length vertex-list) 2)
    nil  ; Return failure if not enough vertices
    (progn
      
      ;; Get first two vertices (for start direction)
      (setq first-pt (car vertex-list))
      (setq second-pt (cadr vertex-list))
      
      ;; Get last two vertices (for end direction)
      (setq last-pt (last vertex-list))
      (setq second-last-pt (nth (- (length vertex-list) 2) vertex-list))
      
      ;; Calculate direction vector at start (from second point to first point)
      ;; This gives us the direction to extend the start
      (setq dir-vec1 (list 
                       (- (car first-pt) (car second-pt))      ; X component
                       (- (cadr first-pt) (cadr second-pt))    ; Y component
                     ))
      
      ;; Calculate magnitude for normalization
      (setq mag1 (distance first-pt second-pt))
      
      ;; Check for zero-length first segment
      (if (< mag1 0.0001)
        (progn
          (princ "\n  Warning: Zero-length segment at start of polyline, skipping...")
          (setq mag1 nil)
        )
        ;; Normalize direction vector at start
        (setq dir-vec1 (list 
                         (/ (car dir-vec1) mag1)
                         (/ (cadr dir-vec1) mag1)
                       ))
      )
      
      ;; Calculate direction vector at end (from second-to-last to last point)
      ;; This gives us the direction to extend the end
      (setq dir-vec2 (list 
                       (- (car last-pt) (car second-last-pt))      ; X component
                       (- (cadr last-pt) (cadr second-last-pt))    ; Y component
                     ))
      
      ;; Calculate magnitude for normalization
      (setq mag2 (distance last-pt second-last-pt))
      
      ;; Check for zero-length last segment
      (if (< mag2 0.0001)
        (progn
          (princ "\n  Warning: Zero-length segment at end of polyline, skipping...")
          (setq mag2 nil)
        )
        ;; Normalize direction vector at end
        (setq dir-vec2 (list 
                         (/ (car dir-vec2) mag2)
                         (/ (cadr dir-vec2) mag2)
                       ))
      )
      
      ;; Check if either segment was degenerate
      (if (or (not mag1) (not mag2))
        nil  ; Return failure if either end has zero-length segment
        (progn
          ;; Calculate new first vertex position
          (setq new-first (list 
                            (+ (car first-pt) (* (car dir-vec1) len))      ; New X
                            (+ (cadr first-pt) (* (cadr dir-vec1) len))    ; New Y
                          ))
          
          ;; Calculate new last vertex position
          (setq new-last (list 
                           (+ (car last-pt) (* (car dir-vec2) len))      ; New X
                           (+ (cadr last-pt) (* (cadr dir-vec2) len))    ; New Y
                         ))
          
          ;; Find and replace the first vertex (first occurrence of DXF code 10)
          (setq obj (subst (cons 10 new-first) 
                           (assoc 10 obj) 
                           obj))
          
          ;; Find and replace the last vertex
          ;; We need to find the last occurrence of DXF code 10
          (setq obj (reverse obj))  ; Reverse to access last vertex first
          (setq obj (subst (cons 10 new-last) 
                           (assoc 10 obj) 
                           obj))
          (setq obj (reverse obj))  ; Reverse back to original order
          
          ;; Write modified entity data back to drawing database
          (entmod obj)
          
          ;; Update display
          (entupd ent)
          
          ;; Return success
          T
        ) ;; end progn - valid segments
      ) ;; end if - checking for degenerate segments
      
      ;; Return success
      T
    )
  )
)


;;; ============================================================================
;;; Function: LENGTHEN-POLYLINE
;;; Purpose: Lengthen or shorten both ends of a POLYLINE or 3DPOLY object
;;; Arguments: 
;;;   ent - Entity name of the POLYLINE/3DPOLY
;;;   len - Length to add (positive) or subtract (negative)
;;; Returns: T on success, nil on failure
;;; Notes: Heavy polylines (POLYLINE) store vertices as separate entities
;;;        This function works with both 2D POLYLINE and 3DPOLY
;;; ============================================================================
(defun lengthen-polyline (ent len / obj vertex-list vertex-ent vertex-data pt 
                           first-pt second-pt last-pt second-last-pt
                           first-ent last-ent dir-vec1 dir-vec2 mag1 mag2 
                           new-first new-last is-3d first-z second-z last-z second-last-z)
  
  ;; Get main polyline entity data
  (setq obj (entget ent))
  
  ;; Initialize vertex list
  (setq vertex-list '())
  
  ;; Get first vertex entity (next entity after main polyline)
  (setq vertex-ent (entnext ent))
  
  ;; Loop through all vertex entities
  ;; Vertices are stored as separate entities with type "VERTEX"
  (while (and vertex-ent
              (= (cdr (assoc 0 (entget vertex-ent))) "VERTEX"))
    
    ;; Get vertex entity data
    (setq vertex-data (entget vertex-ent))
    
    ;; Extract vertex point (DXF code 10)
    (setq pt (cdr (assoc 10 vertex-data)))
    
    ;; Add to vertex list (store both point and entity name)
    (setq vertex-list (append vertex-list (list (cons pt vertex-ent))))
    
    ;; Move to next entity
    (setq vertex-ent (entnext vertex-ent))
  )
  
  ;; Check if we have at least 2 vertices
  (if (< (length vertex-list) 2)
    nil  ; Return failure if not enough vertices
    (progn
      
      ;; Extract first two points and their entity names
      (setq first-pt (car (car vertex-list)))
      (setq first-ent (cdr (car vertex-list)))
      (setq second-pt (car (cadr vertex-list)))
      
      ;; Extract last two points and entity names
      (setq last-pt (car (last vertex-list)))
      (setq last-ent (cdr (last vertex-list)))
      (setq second-last-pt (car (nth (- (length vertex-list) 2) vertex-list)))
      
      ;; Check if polyline is 3D (has Z coordinates)
      (setq is-3d (and (caddr first-pt) (caddr second-pt) (caddr last-pt) (caddr second-last-pt)))
      
      ;; Set Z values for calculations (0.0 if 2D)
      (setq first-z (if (caddr first-pt) (caddr first-pt) 0.0))
      (setq second-z (if (caddr second-pt) (caddr second-pt) 0.0))
      (setq last-z (if (caddr last-pt) (caddr last-pt) 0.0))
      (setq second-last-z (if (caddr second-last-pt) (caddr second-last-pt) 0.0))
      
      ;; Calculate direction vector at start (from second to first vertex)
      (setq dir-vec1 (list 
                       (- (car first-pt) (car second-pt))        ; X component
                       (- (cadr first-pt) (cadr second-pt))      ; Y component
                       (- first-z second-z)                       ; Z component
                     ))
      
      ;; Calculate magnitude for normalization
      (setq mag1 (distance first-pt second-pt))
      
      ;; Normalize direction vector at start (check for zero length)
      (if (< mag1 0.0001)
        (progn
          (princ "\n  Warning: Zero-length segment at start of polyline, skipping...")
          (setq mag1 nil)
        )
        (setq dir-vec1 (list 
                         (/ (car dir-vec1) mag1)
                         (/ (cadr dir-vec1) mag1)
                         (/ (caddr dir-vec1) mag1)
                       ))
      )
      
      ;; Calculate direction vector at end (from second-to-last to last vertex)
      (setq dir-vec2 (list 
                       (- (car last-pt) (car second-last-pt))        ; X component
                       (- (cadr last-pt) (cadr second-last-pt))      ; Y component
                       (- last-z second-last-z)                       ; Z component
                     ))
      
      ;; Calculate magnitude for normalization
      (setq mag2 (distance last-pt second-last-pt))
      
      ;; Normalize direction vector at end (check for zero length)
      (if (< mag2 0.0001)
        (progn
          (princ "\n  Warning: Zero-length segment at end of polyline, skipping...")
          (setq mag2 nil)
        )
        (setq dir-vec2 (list 
                         (/ (car dir-vec2) mag2)
                         (/ (cadr dir-vec2) mag2)
                         (/ (caddr dir-vec2) mag2)
                       ))
      )
      
      ;; Check if either segment was degenerate
      (if (or (not mag1) (not mag2))
        nil  ; Return failure if either end has zero-length segment
        (progn
          ;; Calculate new first vertex position (2D or 3D based on original)
          (if is-3d
            (setq new-first (list 
                              (+ (car first-pt) (* (car dir-vec1) len))          ; New X
                              (+ (cadr first-pt) (* (cadr dir-vec1) len))        ; New Y
                              (+ first-z (* (caddr dir-vec1) len))               ; New Z
                            ))
            (setq new-first (list 
                              (+ (car first-pt) (* (car dir-vec1) len))          ; New X
                              (+ (cadr first-pt) (* (cadr dir-vec1) len))        ; New Y
                            ))
          )
          
          ;; Calculate new last vertex position (2D or 3D based on original)
          (if is-3d
            (setq new-last (list 
                             (+ (car last-pt) (* (car dir-vec2) len))          ; New X
                             (+ (cadr last-pt) (* (cadr dir-vec2) len))        ; New Y
                             (+ last-z (* (caddr dir-vec2) len))               ; New Z
                           ))
            (setq new-last (list 
                             (+ (car last-pt) (* (car dir-vec2) len))          ; New X
                             (+ (cadr last-pt) (* (cadr dir-vec2) len))        ; New Y
                           ))
          )
          
          ;; Update first vertex
          (setq vertex-data (entget first-ent))
          (setq vertex-data (subst (cons 10 new-first) 
                                   (assoc 10 vertex-data) 
                                   vertex-data))
          (entmod vertex-data)
          (entupd first-ent)
          
          ;; Update last vertex
          (setq vertex-data (entget last-ent))
          (setq vertex-data (subst (cons 10 new-last) 
                                   (assoc 10 vertex-data) 
                                   vertex-data))
          (entmod vertex-data)
          (entupd last-ent)
          
          ;; Update main polyline entity to refresh display
          (entupd ent)
          
          ;; Return success
          T
        ) ;; end progn - valid segments
      ) ;; end if - checking for degenerate segments
    )
  )
)


;;; ============================================================================
;;; End of BATCHLENGTHEN.LSP
;;; ============================================================================

(princ "\n*** BATCHLENGTHEN.LSP loaded successfully ***")
(princ "\nType BATCHLENGTHEN to run the command.")
(princ)
