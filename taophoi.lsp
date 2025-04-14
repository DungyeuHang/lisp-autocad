(defun c:MakePhoiDXF (/ doc layman layers lay basePt targetPt ssOld ssNew minpt maxpt minpt2 maxpt2 newFileName)
  (vl-load-com)

  (prompt "\n👉 Bắt đầu xử lý tạo phôi...")

  ;; 1. Lấy document và danh sách layer
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq layman (vla-get-layers doc))

  ;; 2. Ẩn các layer không mong muốn
  (setq layers '("_mss.dut" "_mss.cat" "_mss.phantom" "_mss.pro" "_mss.canh" "DUNGX"))
  (foreach layer layers
    (setq lay (vl-catch-all-apply 'vla-item (list layman layer)))
    (if (and lay (not (vl-catch-all-error-p lay)))
      (vla-put-layeron lay :vlax-false)
    )
  )

  (command "_.REGENALL")

  ;; 3. Bạn chọn đối tượng làm phôi
  (prompt "\n🖱️ Hãy chọn các đối tượng cần làm phôi: ")
  (setq ssOld (ssget))

  (if (not ssOld)
    (progn
      (prompt "\n⚠️ Không có đối tượng nào được chọn!")
      (exit)
    )
  )

  ;; 4. Copy xuống Y -33333 (vẫn giữ lại đối tượng gốc)
  (setq basePt '(0 0 0))
  (setq targetPt '(0 -33333 0))
  (command "_.COPY" ssOld "" basePt targetPt)

  ;; 5. Tính bounding box đối tượng gốc
  (vla-getboundingbox (vlax-ename->vla-object (ssname ssOld 0)) 'minpt 'maxpt)
  (setq minpt (vlax-safearray->list minpt))
  (setq maxpt (vlax-safearray->list maxpt))

  ;; Dịch bounding box xuống -33333
  (setq minpt2 (list (car minpt) (- (cadr minpt) 33333) 0))
  (setq maxpt2 (list (car maxpt) (- (cadr maxpt) 33333) 0))

  ;; 6. Chọn lại đối tượng vừa copy
  (setq ssNew (ssget "_W" minpt2 maxpt2))

  (if ssNew
    (progn
      ;; 7. Nhập tên file .dxf
      (initget 1)
      (setq newFileName (getstring "\n📁 Nhập tên file DXF (không cần .dxf): "))

      ;; 8. Ghi WBLOCK thành file DWG tạm thời
      ;; 8. Ghi WBLOCK thành file DWG tạm thời
      (setq tempFile (strcat (getvar "DWGPREFIX") newFileName "_temp.dwg"))

      ;; Tạo danh sách entity name từ ssNew
      (setq entList '())
      (repeat (setq i (sslength ssNew))
        (setq i (1- i))
        (setq entList (cons (ssname ssNew i) entList))
      )

      ;; Thực hiện WBLOCK
      (apply 'command
            (append
              (list "-WBLOCK" tempFile "") ; tên file và điểm gốc
              entList ; danh sách các entity
              (list "") ; kết thúc
            )
      )


      ;; 9. Mở file tạm và Save As thành DXF 2007
      (prompt "\n💾 Đang chuyển đổi sang DXF 2007...")
      (setq acadApp (vlax-get-acad-object))
      (setq docs (vla-get-documents acadApp))
      (setq tempDoc (vla-open docs tempFile))
      (vla-saveas tempDoc (strcat (getvar "DWGPREFIX") newFileName ".dxf") ac2007_dxf)
      (vla-close tempDoc)

      ;; 10. Xóa file tạm
      (vl-file-delete tempFile)

(prompt (strcat "\n✅ Đã tạo file: " newFileName ".dxf và xoá file tạm .dwg"))


      (prompt (strcat "\n✅ Đã tạo file: " newFileName ".dxf"))
    )
    (prompt "\n⚠️ Không tìm thấy vùng copy để tạo file DXF.")
  )

  (princ)
)
