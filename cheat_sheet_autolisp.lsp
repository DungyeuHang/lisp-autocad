from pathlib import Path

# Nội dung AutoLISP Cheat Sheet gói trong comment block để lưu dưới dạng .lsp
cheat_sheet_lisp = """
;;; ==========================
;;; 📘 AUTO LISP CHEAT SHEET
;;; ==========================

;;; 🔧 Định nghĩa lệnh mới
;;; (defun c:tenlenh ( / bien1 bien2 ) ...)
;;; - 'c:tenlenh' → gõ trong AutoCAD là TENLENH
;;; - Biến sau dấu '/' là biến cục bộ

;;; 📥 Hàm nhập dữ liệu
;;; (getpoint)   ; Chọn điểm trên màn hình
;;; (getint)     ; Nhập số nguyên
;;; (getreal)    ; Nhập số thực
;;; (getstring)  ; Nhập chuỗi
;;; (getkword)   ; Nhập từ khóa (có gợi ý lựa chọn)

;;; 📤 Hàm xuất dữ liệu
;;; (princ)      ; In ra màn hình (yên lặng)
;;; (prompt)     ; In ra thông báo
;;; (print)      ; In ra và xuống dòng

;;; 📦 Xử lý danh sách
;;; (car lst)     ; Phần tử đầu (first)
;;; (cdr lst)     ; Phần còn lại (rest)
;;; (cons a b)    ; Ghép phần tử a vào đầu b
;;; (list a b)    ; Tạo danh sách từ a, b
;;; (append a b)  ; Ghép 2 danh sách

;;; 🧮 Toán học
;;; (+ a b) (- a b) (* a b) (/ a b)
;;; (abs x) (sqrt x) (expt a b)

;;; 🎯 AutoCAD functions
;;; (entget ename)   ; Lấy dữ liệu 1 entity
;;; (entmod data)    ; Cập nhật dữ liệu entity
;;; (entupd ename)   ; Làm mới hiển thị entity
;;; (ssget)          ; Chọn đối tượng (selection set)
;;; (ssname ss i)    ; Lấy phần tử thứ i từ ss
;;; (sslength ss)    ; Đếm số đối tượng trong ss

;;; ⛑ Xử lý điểm 3D
;;; (car pt)    ; X
;;; (cadr pt)   ; Y
;;; (caddr pt)  ; Z
;;; (list x y z); Tạo điểm mới

;;; ==========================
;;; ✅ Dành cho người cần nhớ nhanh sau 1 thời gian quên Lisp!
;;; ==========================

(princ "\\n✅ Đã load Cheat Sheet Lisp! Gõ lệnh CHS để xem.")

(defun c:CHS ( )
  (prompt "\\n✅ Xem nội dung cheat sheet trong file .lsp bạn vừa load.")
  (princ)
)
"""

# Tạo file .lsp
file_path = Path("/mnt/data/cheat_sheet_autolisp.lsp")
file_path.write_text(cheat_sheet_lisp, encoding="utf-8")
file_path.name


; =========================================
; 🚀 AUTO LISP CHEAT SHEET - FULL VERSION 🚀
; Tác giả: ChatGPT & [Tên bạn]
; Lệnh khởi động: CHS (hiện bảng này)
; =========================================

(defun c:CHS ()
  (princ "\n📘 AUTO LISP CHEAT SHEET 📘")
  (princ "\n----------------------------------")
  (princ "\n📌 Biến và Hàm Cơ Bản:")
  (princ "\n - setq: gán giá trị (setq a 1)")
  (princ "\n - defun: định nghĩa hàm")
  (princ "\n - if / cond: câu điều kiện")
  (princ "\n - while / repeat: vòng lặp")
  (princ "\n - progn: chạy nhiều biểu thức")

  (princ "\n\n📌 Danh sách & chuỗi:")
  (princ "\n - list: tạo danh sách (list 1 2 3)")
  (princ "\n - car / cdr: phần tử đầu / còn lại")
  (princ "\n - nth / length / append / reverse")

  (princ "\n\n📌 Tọa độ và điểm:")
  (princ "\n - (list x y z): tạo điểm")
  (princ "\n - (car pt), (cadr pt), (caddr pt): trích x, y, z")
  (princ "\n - polar, distance, angle")

  (princ "\n\n📌 Xử lý đối tượng:")
  (princ "\n - ssget: chọn đối tượng")
  (princ "\n - ssname, sslength")
  (princ "\n - entget: lấy dữ liệu đối tượng")
  (princ "\n - entmod: chỉnh sửa")
  (princ "\n - entupd: cập nhật")

  (princ "\n\n📌 Dữ liệu DXF (assoc code):")
  (princ "\n - 0: loại đối tượng")
  (princ "\n - 10/11/13/14: điểm (LINE, DIM...)")
  (princ "\n - 8: layer")
  (princ "\n - 62: màu")

  (princ "\n\n📌 Hàm tương tác người dùng:")
  (princ "\n - getpoint, getreal, getint")
  (princ "\n - getstring, getkword")
  (princ "\n - initget: giới hạn đầu vào")

  (princ "\n\n📌 Tùy biến thêm:")
  (princ "\n - command: gọi lệnh AutoCAD")
  (princ "\n - vlax-get / vlax-put (ActiveX nâng cao)")
  (princ "\n - alert, princ, prompt")

  (princ "\n\n✅ Gõ lệnh CHS bất cứ lúc nào để xem lại bảng này.")
  (princ)
)
