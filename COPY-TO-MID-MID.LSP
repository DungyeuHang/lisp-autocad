(defun c:CMMM (/ ss basePt done line1 line2 p1a p1b p2a p2b mid1 mid2 destPt vlaLine1 vlaLine2)
  (vl-load-com)

  (prompt "\nChọn các đối tượng cần copy: ")
  (setq ss (ssget))

  (if ss
    (progn
      (prompt "\nChọn điểm gốc để copy: ")
      (setq basePt (getpoint))

      (setq done nil)

      (while (not done)
        (prompt "\n--- Chọn cặp LINE mới (Enter để thoát) ---")

        ;; Chọn LINE thứ nhất
        (setq line1 (car (entsel "\nChọn LINE thứ nhất: ")))

        ;; Nếu không chọn gì thì thoát vòng lặp
        (if (not line1)
          (setq done T)
          (progn
            ;; Chọn LINE thứ hai
            (setq line2 (car (entsel "\nChọn LINE thứ hai: ")))

            (if (and line1 line2)
              (progn
                ;; Lấy object VLA
                (setq vlaLine1 (vlax-ename->vla-object line1))
                (setq vlaLine2 (vlax-ename->vla-object line2))

                ;; Tính midpoint của LINE 1
                (setq p1a (vlax-curve-getStartPoint vlaLine1))
                (setq p1b (vlax-curve-getEndPoint vlaLine1))
                (setq mid1 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1a p1b))

                ;; Tính midpoint của LINE 2
                (setq p2a (vlax-curve-getStartPoint vlaLine2))
                (setq p2b (vlax-curve-getEndPoint vlaLine2))
                (setq mid2 (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p2a p2b))

                ;; Tính điểm đến (mid của mid1 và mid2)
                (setq destPt (mapcar '(lambda (a b) (/ (+ a b) 2.0)) mid1 mid2))

                ;; COPY đối tượng
                (command "_COPY" ss "" basePt destPt)
              )
              (prompt "\nCần chọn đủ 2 LINE!")
            )
          )
        )
      )
    )
    (prompt "\nKhông có đối tượng nào được chọn.")
  )
  (princ)
)
