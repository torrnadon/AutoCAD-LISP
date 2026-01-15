(defun c:3DFACEOUTLINE (/ ss i ent entdata p1 p2 p3 p4 pts newss lastent)
  (vl-load-com)
  (princ "\nSelect 3D Face objects...")
  
  ;; Get selection set of 3D Face objects
  (setq ss (ssget '((0 . "3DFACE"))))
  
  (if ss
    (progn
      ;; Begin undo group
      (command "_.UNDO" "BE")
      
      (setq i 0)
      (setq newss (ssadd)) ;; Create new selection set for created objects
      (princ (strcat "\nProcessing " (itoa (sslength ss)) " 3D Face(s)..."))
      
      ;; Loop through each selected 3D Face
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq entdata (entget ent))
        
        ;; Extract vertices from 3D Face
        ;; 3D Face has 4 vertices (DXF codes 10, 11, 12, 13)
        (setq p1 (cdr (assoc 10 entdata)))
        (setq p2 (cdr (assoc 11 entdata)))
        (setq p3 (cdr (assoc 12 entdata)))
        (setq p4 (cdr (assoc 13 entdata)))
        
        ;; Check if it's a triangle (p3 = p4) or quadrilateral
        (if (equal p3 p4 0.0001)
          ;; Triangle: use only 3 points
          (setq pts (list p1 p2 p3))
          ;; Quadrilateral: use all 4 points
          (setq pts (list p1 p2 p3 p4))
        )
        
        ;; Create 3D polyline along the border
        (command "_.3DPOLY")
        (foreach pt pts
          (command pt)
        )
        (command "C") ;; Close the polyline
        
        ;; Add the newly created polyline to selection set
        (setq lastent (entlast))
        (ssadd lastent newss)
        
        (setq i (1+ i))
      )
      
      ;; End undo group
      (command "_.UNDO" "E")
      
      (princ (strcat "\n" (itoa i) " outline polyline(s) created and selected."))
      
      ;; Select the created objects
      (sssetfirst nil newss)
    )
    (princ "\nNo 3D Face objects selected.")
  )
  
  (princ)
)

;; Draw only the bottom line (edge with minimal Z coordinate at center)
(defun c:3DFACEBOTTOMLINE (/ ss i ent entdata p1 p2 p3 p4 pts edges centerz minedge newss lastent j edge edgenum centerpt)
  (vl-load-com)
  (princ "\nSelect 3D Face objects...")
  
  ;; Get selection set of 3D Face objects
  (setq ss (ssget '((0 . "3DFACE"))))
  
  (if ss
    (progn
      ;; Begin undo group
      (command "_.UNDO" "BE")
      
      (setq i 0)
      (setq newss (ssadd)) ;; Create new selection set for created objects
      (princ (strcat "\nProcessing " (itoa (sslength ss)) " 3D Face(s)..."))
      
      ;; Loop through each selected 3D Face
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq entdata (entget ent))
        
        (princ (strcat "\n\n=== 3D Face #" (itoa (1+ i)) " ==="))
        
        ;; Extract vertices from 3D Face
        (setq p1 (cdr (assoc 10 entdata)))
        (setq p2 (cdr (assoc 11 entdata)))
        (setq p3 (cdr (assoc 12 entdata)))
        (setq p4 (cdr (assoc 13 entdata)))
        
        ;; Check if it's a triangle or quadrilateral
        (if (equal p3 p4 0.0001)
          (setq pts (list p1 p2 p3))
          (setq pts (list p1 p2 p3 p4))
        )
        
        (princ (strcat "\nType: " (if (= (length pts) 3) "Triangle" "Quadrilateral")))
        
        ;; Build list of edges with their center Z values
        (setq edges '())
        (setq j 0)
        (setq edgenum 1)
        (repeat (length pts)
          (setq edge (list
                      (nth j pts)
                      (nth (rem (1+ j) (length pts)) pts)
                    ))
          
          ;; Calculate center point of the edge
          (setq centerpt (list
                          (/ (+ (car (car edge)) (car (cadr edge))) 2.0)
                          (/ (+ (cadr (car edge)) (cadr (cadr edge))) 2.0)
                          (/ (+ (caddr (car edge)) (caddr (cadr edge))) 2.0)
                        ))
          
          ;; Get Z coordinate of center point
          (setq centerz (caddr centerpt))
          
          ;; Debug output for each edge
          (princ (strcat "\n  Edge " (itoa edgenum) ": "))
          (princ (strcat "P1(" (rtos (car (car edge)) 2 3) ", " 
                               (rtos (cadr (car edge)) 2 3) ", " 
                               (rtos (caddr (car edge)) 2 3) ") -> "))
          (princ (strcat "P2(" (rtos (car (cadr edge)) 2 3) ", " 
                               (rtos (cadr (cadr edge)) 2 3) ", " 
                               (rtos (caddr (cadr edge)) 2 3) ")"))
          (princ (strcat "\n       Center(" (rtos (car centerpt) 2 3) ", " 
                                            (rtos (cadr centerpt) 2 3) ", " 
                                            (rtos (caddr centerpt) 2 3) ")"))
          (princ (strcat " | Center Z: " (rtos centerz 2 3)))
          
          (setq edges (cons (cons centerz edge) edges))
          (setq j (1+ j))
          (setq edgenum (1+ edgenum))
        )
        
        ;; Find the edge with the minimum center Z coordinate
        (setq minedge (car edges))
        (foreach edge edges
          (if (< (car edge) (car minedge))
            (setq minedge edge)
          )
        )
        
        (princ (strcat "\n  >>> BOTTOM EDGE: Center Z = " (rtos (car minedge) 2 3)))
        
        ;; Draw the bottom line
        (command "_.LINE" (cadr minedge) (caddr minedge) "")
        
        ;; Add the newly created line to selection set
        (setq lastent (entlast))
        (ssadd lastent newss)
        
        (setq i (1+ i))
      )
      
      ;; End undo group
      (command "_.UNDO" "E")
      
      (princ (strcat "\n\n" (itoa i) " bottom line(s) created and selected."))
      
      ;; Select the created objects
      (sssetfirst nil newss)
    )
    (princ "\nNo 3D Face objects selected.")
  )
  
  (princ)
)

(princ "\n3D Face Tools Loaded!")
(princ "\nType 3DFACEOUTLINE to create closed polylines")
(princ "\nType 3DFACEBOTTOMLINE to create bottom edge lines")
(princ)