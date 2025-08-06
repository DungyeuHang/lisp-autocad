(defun c:vaa-ve-arc ( / step get-dim-measurement make-rect add-dim ent meas inspt)
  (vl-load-com)
  (setq step 1)
  (setvar "osmode" 16384)

  ;; Hàm lấy measurement từ DIM
  (defun get-dim-measurement (ent)
    (if (and ent (setq entObj (vlax-ename->vla-object ent)))
      (vla-get-measurement entObj)
      nil
    )
  )

  ;; Hàm tạo RECTANG + ghi dim
  (defun make-rect (base-pt len width)
    (setq x (car base-pt))
    (setq y (cadr base-pt))
    (setq p1 (list x y))                             ; góc trái dưới
    (setq p2 (list (+ x len) y))                     ; góc phải dưới
    (setq p3 (list (+ x len) (+ y width)))           ; góc phải trên
    (setq p4 (list x (+ y width)))                   ; góc trái trên

    ;; Vẽ RECTANG
    (setvar "clayer" "_mss.bao")
    (command "RECTANG" p1 p3)
    (setvar "clayer" "_mss.kichthuoc")
 ;; Tạo DIM chiều dài: dưới hình chữ nhật
    (command "DIMLINEAR" p1 p2 (list x (- y 100)) "")

    ;; Tạo DIM chiều rộng: bên phải hình chữ nhật
    (command "DIMLINEAR" p4 p1 (list (- x  100)  y ) "")
  )
  

  ;; Vòng lặp 2 lần
  (repeat 2
    (prompt (strcat "\nLần " (itoa step) ": Chọn đối tượng DIM bất kỳ..."))
    (setq ent (car (entsel)))
    (if (and ent (setq meas (get-dim-measurement ent)))
      (progn
        (setq meas (+ meas 0.2))
        (prompt (strcat "\nChiều dài đo được: " (rtos meas 2 2)))
        (setq inspt (getpoint "\nChọn điểm đặt RECTANG: "))
        (if (= step 1)
          (make-rect inspt meas 48.8)
          (make-rect inspt meas 18.8)
        )
        (setq step (+ step 1))
      )
      (prompt "\n❌ Không phải DIM hợp lệ hoặc không đo được.")
    )
  )
  (setvar "osmode" 16383)

  (princ)
)
