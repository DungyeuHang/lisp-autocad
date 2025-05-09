
(defun c:CHIA-DIM (/ ent entData pt1 pt2 ptDim dirVec dirLen normVec ptList tmpPt i pA pB dimLinePt
                      projectedPtList projectPointOntoLine getNormVec getDirVec getOffsetAlongNormal dimOffset)
  (vl-load-com)

  ;; Hàm dựng vector hướng đoạn
  (defun getDirVec (p1 p2)
    (setq v (mapcar '- p2 p1))
    (setq len (distance p1 p2))
    (if (/= len 0)
      (mapcar '(lambda (x) (/ x len)) v)
      '(1 0)
    )
  )

  ;; Vector pháp tuyến vuông góc
  (defun getNormVec (dirVec)
    (list (- (cadr dirVec)) (car dirVec))
  )

  ;; Chiếu điểm chia lên đoạn gốc pt1–pt2
  (defun projectPointOntoLine (pt basePt dirVec)
    (setq vec (mapcar '- pt basePt))
    (setq dotProd (+ (* (car vec) (car dirVec)) (* (cadr vec) (cadr dirVec))))
    (mapcar '+ basePt (mapcar '(lambda (x) (* x dotProd)) dirVec))
  )

  ;; Tính offset vuông góc từ đoạn đến chữ DIM
  (defun getOffsetAlongNormal (basePt ptDim normVec)
    (setq v (mapcar '- ptDim basePt))
    (+ (* (car v) (car normVec)) (* (cadr v) (cadr normVec)))
  )

  ;; --- Bắt đầu chương trình ---
  (prompt "\nChọn đối tượng DIM cần chia: ")
  (setq ent (car (entsel)))
  (if (and ent (= (cdr (assoc 0 (entget ent))) "DIMENSION"))
    (progn
      ;; Lấy dữ liệu
      (setq entData (entget ent))
      (setq pt1 (cdr (assoc 13 entData)))
      (setq pt2 (cdr (assoc 14 entData)))
      (setq ptDim (cdr (assoc 15 entData)))

      ;; Vector hướng & pháp tuyến
      (setq dirVec (getDirVec pt1 pt2))
      (setq normVec (getNormVec dirVec))
      (setq dimOffset (getOffsetAlongNormal pt1 ptDim normVec))

      ;; Chọn các điểm chia → chiếu lên đoạn chính
      (prompt "\nChọn các điểm chia (Enter để kết thúc): ")
      (setq ptList nil)
      (while (setq tmpPt (getpoint "\nChọn điểm chia: "))
        (setq projectedPt (projectPointOntoLine tmpPt pt1 dirVec))
        (setq ptList (append ptList (list projectedPt)))
      )

      ;; Thêm đầu/cuối và sắp xếp
      (setq ptList (append (list pt1) ptList (list pt2)))
      (setq ptList (vl-sort ptList '(lambda (a b) (< (distance pt1 a) (distance pt1 b)))))

      ;; Xoá DIM cũ
      (entdel ent)

      ;; Vẽ DIM mới
      (setq i 0)
      (while (< i (1- (length ptList)))
        (setq pA (nth i ptList))
        (setq pB (nth (1+ i) ptList))
        (setq midPt (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pA pB))
        (setq dimLinePt (mapcar '+ midPt (mapcar '(lambda (x) (* x dimOffset)) normVec)))
        (command "_.DIMLINEAR" "_non" pA "_non" pB "_non" ptDim)
        (setq i (1+ i))
      )
    )
    (prompt "\nKhông phải đối tượng DIM hợp lệ.")
  )
  (princ)
)
