(vl-load-com)

(defun c:UFF ( / ent obj n i p1 p2 p0 p3 ang1 ang2 ipt pts)
  (setvar "clayer" "_mss.phantom")
  (setq ent (car (entsel "\nChọn polyline cần bỏ fillet: ")))
  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (if (= (vla-get-ObjectName obj) "AcDbPolyline")
        (progn
          (setq n (1+ (fix (vlax-curve-getEndParam ent))))
          (setq pts '())

          ;; duyệt từng đoạn
          (setq i 0)
          (while (< i n)
            (setq bulge (vla-GetBulge obj i))
            (if (= bulge 0.0)
              ;; không có fillet, giữ nguyên điểm
              (setq pts (append pts (list (vlax-curve-getPointAtParam ent i))))
              ;; có fillet -> tính giao hai đoạn thẳng
              (progn
                ;; p0 = điểm trước fillet
                (setq p0 (vlax-curve-getPointAtParam ent i))
                ;; p3 = điểm sau fillet
                (setq p3 (vlax-curve-getPointAtParam ent (1+ i)))
                ;; p1 = điểm trước nữa (i-1)
                (setq p1 (vlax-curve-getPointAtParam ent (max 0 (- i 1))))
                ;; p2 = điểm sau nữa (i+2) nếu có
                (setq p2 (if (< (+ i 2) n) (vlax-curve-getPointAtParam ent (+ i 2)) p3))

                ;; tính vector để tìm giao
                (setq ang1 (angle p1 p0))
                (setq ang2 (angle p3 p2))

                ;; tìm giao của 2 đường thẳng p1->p0 và p3->p2
                (setq ipt (inters p0 (polar p0 ang1 1.0) p3 (polar p3 ang2 1.0) nil))

                ;; thêm p0 (đầu thẳng), thêm giao ipt, không thêm cung
                (setq pts (append pts (list p0 ipt)))
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
          (princ "\n>>> Đã bỏ fillet, tạo polyline mới với góc nhọn! <<<")
        )
        (princ "\nĐối tượng không phải polyline!")
      )
    )
  )
  (princ)
)
