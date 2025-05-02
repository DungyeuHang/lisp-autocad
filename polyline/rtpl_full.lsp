
(defun nthcdr (n lst)
  (if (or (<= n 0) (null lst))
    lst
    (nthcdr (1- n) (cdr lst))))

(defun take (lst n)
  (if (or (null lst) (= n 0)) '() (cons (car lst) (take (cdr lst) (1- n)))))

(defun rotate-list (lst n)
  (append (nthcdr n lst) (reverse (reverse (take lst n)))))

;Đổi điểm đầu polyline
(defun c:RTPL (/ ent obj coords ptList index pt startPt closestDist i newCoords startIndex bulges)

  (setq ent (car (entsel "\n🎯 Chọn polyline cần đổi điểm đầu: ")))
  (if (and ent (= (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
    (progn
      (setq obj (vlax-ename->vla-object ent))
      (setq coords (vlax-get obj 'Coordinates))
      (setq ptList '())
      (setq index 0)
      ;; Tạo danh sách điểm từ coords
      (while (< index (length coords))
        (setq pt (list (nth index coords) (nth (+ index 1) coords)))
        (setq ptList (append ptList (list pt)))
        (setq index (+ index 2)))

      ;; 🔁 Lấy bulge gốc trước khi sửa
      (setq bulges '())
      (setq i 0)
      (repeat (/ (length coords) 2)
        (setq bulges (append bulges (list (vla-GetBulge obj i))))
        (setq i (1+ i)))

      ;; Chọn điểm tham chiếu
      (setq startPt (getpoint "\n📌 Chọn điểm muốn đặt làm điểm đầu: "))

      ;; Tìm chỉ số điểm gần nhất
      (setq closestDist 1e99)
      (setq i 0)
      (foreach tmpPt ptList
        (if (< (distance tmpPt startPt) closestDist)
          (progn
            (setq closestDist (distance tmpPt startPt))
            (setq startIndex i)))
        (setq i (1+ i)))

      ;; Xoay danh sách điểm và bulge theo điểm đầu mới
      (setq ptList (rotate-list ptList startIndex))
      (setq bulges (rotate-list bulges startIndex))

      ;; Gán lại vào polyline
      (setq newCoords (apply 'append ptList))
      (vlax-put obj 'Coordinates newCoords)

      (setq i 0)
      (foreach b bulges
        (vla-SetBulge obj i b)
        (setq i (1+ i)))

      (prompt "\n✅ Đã đổi điểm đầu polyline và giữ nguyên cung."))
    (prompt "\n❌ Không phải là LWPOLYLINE!"))
  (princ))
