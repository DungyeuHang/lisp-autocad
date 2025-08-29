;;; =======================
;;; Copy From Source Center To Centers of Selected Circles
;;; Command: CTC
;;; Phiên bản: không dùng biến 1 ký tự (i, n, d, v.v.)
;;; =======================

(defun c2c-mid (ptA ptB)
  (mapcar '(lambda (a b) (/ (+ a b) 2.0)) ptA ptB))

(defun c2c-bbox-center (ent-name / vla-obj sa-min sa-max pt-min pt-max)
  (if (setq vla-obj (vlax-ename->vla-object ent-name))
    (progn
      (vla-getboundingbox vla-obj 'sa-min 'sa-max)
      (setq pt-min (vlax-safearray->list sa-min)
            pt-max (vlax-safearray->list sa-max))
      (c2c-mid pt-min pt-max)
    )
  )
)

(defun c2c-obj-center (ent-name / ent-data ent-type)
  (setq ent-data (entget ent-name)
        ent-type (cdr (assoc 0 ent-data)))
  (cond
    ((member ent-type '("CIRCLE" "ARC" "ELLIPSE" "POINT"))
      (cdr (assoc 10 ent-data)))                 ; tâm/điểm
    ((= ent-type "LINE")
      (c2c-mid (cdr (assoc 10 ent-data)) (cdr (assoc 11 ent-data))))
    ((= ent-type "INSERT")
      (cdr (assoc 10 ent-data)))                 ; điểm chèn block
    ((member ent-type '("LWPOLYLINE" "POLYLINE" "SPLINE" "REGION"))
      (c2c-bbox-center ent-name))                ; tâm bbox (xấp xỉ)
    (T (c2c-bbox-center ent-name))
  )
)

(defun C:CTC ( / c2c-old-cmdecho c2c-picked c2c-entity-src c2c-source-center
                     c2c-ss-circles c2c-idx c2c-count
                     c2c-entity-cir c2c-dest-center)

  (defun *error* (c2c-msg)
    (if (and c2c-msg (not (wcmatch (strcase c2c-msg) "*BREAK,*CANCEL*,*EXIT*")))
      (princ (strcat "\n[!] Lỗi: " c2c-msg))
    )
    (if c2c-old-cmdecho (setvar 'CMDECHO c2c-old-cmdecho))
    (command "_.UNDO" "_END")
    (princ)
  )

  (setq c2c-old-cmdecho (getvar 'CMDECHO))
  (setvar 'CMDECHO 0)
  (command "_.UNDO" "_GROUP")

  ;; 1) Chọn đối tượng nguồn
  (prompt "\nChọn đối tượng 1 cần COPY: ")
  (while (not (setq c2c-picked (entsel)))
    (prompt "\n[!] Hãy chọn 1 đối tượng.")
  )
  (setq c2c-entity-src (car c2c-picked))

  ;; 2) Lấy tâm nguồn (auto → fallback chọn tay)
  (setq c2c-source-center (c2c-obj-center c2c-entity-src))
  (if (not (and c2c-source-center (= (length c2c-source-center) 3)))
    (progn
      (prompt "\nKhông xác định được tâm tự động. Hãy chỉ điểm tâm nguồn.")
      (setq c2c-source-center (getpoint "\nChọn tâm nguồn: "))
    )
  )
  (if (not c2c-source-center) (progn (*error* "Không có tâm nguồn.") (exit)))

  ;; 3) Quét chọn các CIRCLE đích
  (prompt "\nChọn các CIRCLE đích (quét vùng, nhiều lần được): ")
  (setq c2c-ss-circles (ssget "_:L" '((0 . "CIRCLE"))))
  (if (not c2c-ss-circles)
    (progn
      (prompt "\n[!] Không có CIRCLE nào được chọn.")
      (*error* nil)
      (exit)
    )
  )

  ;; 4) Lặp copy tới từng tâm CIRCLE
  (setq c2c-count (sslength c2c-ss-circles)
        c2c-idx   0)
  (while (< c2c-idx c2c-count)
    (setq c2c-entity-cir (ssname c2c-ss-circles c2c-idx)
          c2c-dest-center (cdr (assoc 10 (entget c2c-entity-cir))))
    (if (and c2c-dest-center (not (equal c2c-dest-center c2c-source-center 1e-8)))
      (command "_.COPY" c2c-entity-src "" c2c-source-center c2c-dest-center)
    )
    (setq c2c-idx (1+ c2c-idx))
  )

  (prompt (strcat "\nĐã copy tới " (itoa c2c-count) " tâm CIRCLE."))
  (setvar 'CMDECHO c2c-old-cmdecho)
  (command "_.UNDO" "_END")
  (princ)
)
