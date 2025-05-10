(defun c:vss1 (/ i ent1 dd dk keyword diemtam doituong sls_1 sls_test tam_lo_song kctam sl sosong1 diemdim1 diemdim2 diemdim3 diemdim4 diemdim5 diemdim6 diemdim7 diemdim8 diemtext ename ent doituong1 bbox minPt maxPt diemtam themObj)
(command "osmode" "16383")
(initget 1 "tr th v") ;tron thoi vuong
  (setq keyword (getkword "Ban muon ve song tron <tr>, thoi <th> hay song vuong <v>"))
  (cond
    ((= keyword "tr") ;tron r=17.25
     (setq dk 17.25)) ; ket thuc dk1
    ((= keyword "th") ;thoi r= 20.75
     (setq dk 20.75 )) ; ket thuc dk2
    ((= keyword "v") ;vuong  = 15.5
     (setq dk 15.5 )) ; ket thuc dk3
     ) ;ket thuc cond
  (while (not dd) ; lặp đến khi có giá trị chiều dài
    (setq ent1 (car (entsel "\nChon doi tuong DIM: ")))
    (if ent1
      (if (= (cdr (assoc 0 (entget ent1))) "DIMENSION")
        (progn
          (setq dd (vla-get-Measurement (vlax-ename->vla-object ent1)))
          (princ (strcat "\nDo dai DIM: " (rtos dd 2 2)))
        )
        (prompt "\n❌ Khong phai DIM. Vui long chon lai.")
      )
      (prompt "\n⚠️ Khong chon gi ca. Vui long chon lai.")
    )
  )
  

; Bước 1: Chọn đối tượng đầu tiên để lấy tâm
  (prompt "\nChọn đối tượng đầu tiên để xác định tâm...")
  (setq doituong1 (ssget "_+.:S")) ; chọn 1 đối tượng

  (if (and doituong1 (= (sslength doituong1) 1))
    (progn
      (setq ename (ssname doituong1 0))
      (setq ent (entget ename))
      (setq type (cdr (assoc 0 ent))) ; lấy loại đối tượng

      ;; Xác định tọa độ tâm dựa theo loại đối tượng
      (cond
        ((= type "CIRCLE")
         (setq diemtam (cdr (assoc 10 ent)))
        )
        ((= type "INSERT") ; BLOCK
         (setq diemtam (cdr (assoc 10 ent))) ; điểm chèn block
        )
        ((= type "LWPOLYLINE")
         (setq bbox (vla-getboundingbox (vlax-ename->vla-object ename) 'minPt 'maxPt))
         (setq minPt (vlax-safearray->list minPt))
         (setq maxPt (vlax-safearray->list maxPt))
         (setq diemtam (mapcar '(lambda (a b) (/ (+ a b) 2.0)) minPt maxPt)) ; tâm bbox
        )
        (T (prompt "\nLoại đối tượng không được hỗ trợ."))
      )

      ;; In tọa độ tâm ra màn hình
      (if diemtam
        (progn
          (princ (strcat "\nTâm của đối tượng là: "
                         (rtos (car diemtam) 2 3) ", "
                         (rtos (cadr diemtam) 2 3)))

          ;; Bước 2: Chọn thêm các đối tượng khác (tùy chọn)
          (prompt "\nChọn các đối tượng khác (nếu có), nhấn ENTER nếu không có...")
          (setq themObj (ssget)) ; chọn nhiều đối tượng hoặc bỏ qua

          ;; Gộp doituong1 và các đối tượng thêm vào thành 1 selection set mới
          (cond
            (themObj
              ;; Tạo selection set mới chứa cả doituong1 và themObj
              (setq doituong (ssadd (ssname doituong1 0)))
              (repeat (sslength themObj)
                (setq doituong (ssadd (ssname themObj (setq i (if i (1+ i) 0))) doituong))
              )
            )
            (T
              (setq doituong doituong1)
            )
          )

          ;; Bạn có thể tiếp tục xử lý với tập `doituong` ở đây
          (princ (strcat "\nTổng số đối tượng đã chọn: " (itoa (sslength doituong))))
        )
      )
    )
    (prompt "\nVui lòng chọn đúng 1 đối tượng đầu tiên.")
  )
  (foreach sls_1 (list 21 19 17 15 13 11 9 7 5 3 )
          (setq sls_test (- sls_1 2))
          (setq tam_lo_song (/ (+ dd (* 2 dk)) (+ sls_test 1)))

    
(if (and (> tam_lo_song 130) (< tam_lo_song 160)) ;if
     (and (setq kctam tam_lo_song)
       (setq sl sls_test)) ;then
        (if (and (> tam_lo_song 100) (< tam_lo_song 130))
          (and (setq kctam tam_lo_song)
          (setq sl sls_test))
        ) ;else
      )
  )

  
  
  (setq sosong1 (+ (fix (/ sl 2)) 1))
  
  (command "osmode" "16384")
  (command "copy" doituong "" diemtam "_array" sosong1 (list (+ (car diemtam) kctam) (cadr diemtam) ) )
  (command "copy" doituong "" diemtam "_array" sosong1 (list (- (car diemtam) kctam) (cadr diemtam) ) )
  (setq diemdim1 (list (+ (car diemtam) kctam) (cadr diemtam))) ;diemdim1 cach diemtam =kctam
  (setq diemdim2 (list (car diemtam) (+ (cadr diemtam) 50))) ;;diemdim2 huong len tren truc y
  (setq diemdim3 (list (+ (+ (car diemtam) (* kctam (- sosong1 1))) dk) (cadr diemtam))) ;diemdim3 ngoai cung ben phai
  (setq diemdim4 (list (- (+ (car diemtam) (* kctam sosong1 )) dk) (cadr diemtam))) ;diemdim4
  (setq diemdim5 (list (- (- (car diemtam) (* kctam (- sosong1 1))) dk) (cadr diemtam))) ;diemdim5 ngoai cung ben phai
  (setq diemdim6 (list (+ (- (car diemtam) (* kctam sosong1 )) dk) (cadr diemtam)))
  (setq diemdim7 (list (+ (+ (car diemtam) (* kctam 2)) dk) (cadr diemtam))) ;diemdim5 ngoai cung ben phai
  (setq diemdim8 (list (- (+ (car diemtam) (* kctam 3)) dk) (cadr diemtam)))

  (setq diemtext (list (car diemtam) (+ (cadr diemtam) 200))) ;diemtext huong len tren truc y

  (command "dimlinear" diemtam diemdim1 diemdim2 )
  (command "dimlinear"  diemdim3 diemdim4 diemdim2)
  (command "dimlinear"  diemdim5 diemdim6 diemdim2)
  (command "dimlinear"  diemdim7 diemdim8 diemdim2)
  (command "text" diemtext 55 "0" (strcat (rtos sl) " SONG"))

  (command "osmode" 16383)
  (print "so song la")
  (print sl)
  (princ)

)