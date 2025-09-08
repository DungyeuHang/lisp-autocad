(defun C:IPP-Insert-PG ( / ent blkObj propList propObj valueWidth valueHeight wrapOnWall blockName insertPoint)

  ;; Nhập phào ôm tường (Enter = 60)
  (initget 6)
  (setq wrapOnWall (getint "\nNhập phào ôm tường <60>: "))
  (if (not wrapOnWall) (setq wrapOnWall 60))

  ;; Lấy kích thước từ DIM
  (setq valueWidth  (_PickDimensionAndGetMeasurement "\nChọn DIM chiều RỘNG: "))
  (setq valueHeight (_PickDimensionAndGetMeasurement "\nChọn DIM chiều CAO: "))

  ;; Tên block
  (setq blockName (strcat "PG-" (itoa wrapOnWall)))

  ;; Điểm chèn
  (setq insertPoint (getpoint "\nChọn điểm chèn block: "))
  (if (not insertPoint) (setq insertPoint (getvar 'lastpoint)))

  ;; Insert block
  (command "_.-INSERT" blockName insertPoint 1.0 1.0 0.0)
  (setq ent (entlast))
  (setq blkObj (vlax-ename->vla-object ent))

  ;; Lấy danh sách property
  (setq propList (vlax-invoke blkObj 'GetDynamicBlockProperties))

  ;; Duyệt qua từng property và set giá trị
  (foreach propObj propList
    (cond
      ((= (strcase (vla-get-PropertyName propObj)) "CAO")
       (vla-put-Value propObj (vlax-make-variant valueHeight 5))
      )
      ((= (strcase (vla-get-PropertyName propObj)) "RONG")
       (vla-put-Value propObj (vlax-make-variant valueWidth 5))
      )
    )
  )

  (vla-Update blkObj)
  ;(command "_.REGEN")
  (princ "\n✔ Đã chèn block và cập nhật tham số động.")
  (princ)
)

;; Hàm phụ lấy measurement từ DIM
(defun _PickDimensionAndGetMeasurement (promptMessage
                                        / pickedEntityName entityType entityObject measuredValue)
  (while (not measuredValue)
    (setq pickedEntityName (car (entsel promptMessage)))
    (cond
      ((null pickedEntityName) (prompt "\n[!] Không chọn gì."))
      ((setq entityType (cdr (assoc 0 (entget pickedEntityName))))
       (if (/= entityType "DIMENSION")
         (prompt "\n[!] Hãy chọn đúng đối tượng DIMENSION.")
         (progn
           (setq entityObject (vlax-ename->vla-object pickedEntityName))
           (setq measuredValue (vla-get-Measurement entityObject))
         )
       ))
    )
  )
  measuredValue
)
