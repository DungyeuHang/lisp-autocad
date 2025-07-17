(vl-load-com)

(defun c:UFF ( / ent obj n i pts p0 p1 p2 p3 ang1 ang2 ipt)
  (setq ent (car (entsel "\nChọn polyline cần bỏ fillet (giữ 1 điểm giao, bỏ cả đầu và cuối cung): ")))
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (if (= (vla-get-ObjectName obj) "AcDbPolyline")
        (progn
          (setq n (1+ (fix (vlax-curve-getEndParam ent))))
          (setq pts '())

          (setq i 0)
          (while (< i n)
            (setq bulge (vla-GetBulge obj i))
            (if (= bulge 0.0)
              ;; không phải cung -> giữ nguyên điểm
              (setq pts (append pts (list (vlax-curve-getPointAtParam ent i))))
              ;; là cung -> chỉ lấy giao, bỏ điểm đầu và cuối
              (progn
                (setq p0 (vlax-curve-getPointAtParam ent i))
                (setq p3 (vlax-curve-getPointAtParam ent (1+ i)))
                (setq p1 (vlax-curve-getPointAtParam ent (max 0 (- i 1))))
                (setq p2 (if (< (+ i 2) n)
                           (vlax-curve-getPointAtParam ent (+ i 2))
                           p3))
                ;; tìm giao hai đường thẳng
                (setq ang1 (angle p1 p0))
                (setq ang2 (angle p3 p2))
                (setq ipt (inters p0 (polar p0 ang1 1.0)
                                  p3 (polar p3 ang2 1.0) nil))
                ;; thêm đúng 1 điểm giao
                (setq pts (append pts (list ipt)))
                ;; nhảy qua luôn điểm kế tiếp (p3) để bỏ nó
                (setq i (1+ i))
              )
            )
            (setq i (1+ i))
          )

          ;; tạo polyline mới
          (setq ms (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
          (setq arr (vlax-make-safearray vlax-vbDouble (cons 0 (1- (* 2 (length pts))))))
          (setq k 0)
          (foreach p pts
            (vlax-safearray-put-element arr (* k 2) (car p))
            (vlax-safearray-put-element arr (1+ (* k 2)) (cadr p))
            (setq k (1+ k))
          )
          (vla-AddLightWeightPolyline ms arr)
          (princ "\n>>> Đã bỏ fillet và chỉ giữ 1 điểm giao, bỏ cả đầu & cuối cung! <<<")
        )
        (princ "\nĐối tượng không phải polyline!")
      )
    )
  )
  (princ)
)
