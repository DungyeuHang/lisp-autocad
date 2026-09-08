;;; ============================================================================
;;;  SSD (STRETCH BY DIM) - AutoLISP
;;;
;;;  SSD  : chon 2 DIM -> L = |DIM1 - DIM2|      -> stretch 1 luot.
;;;  SSD2 : chon 2 DIM -> L = |DIM1 - DIM2| / 2  -> stretch 2 luot
;;;                                                 (cho ket cau doi xung).
;;;  SSDL : nhap L bang tay                      -> stretch 1 luot.
;;;
;;;  Cach dung:
;;;    1. Chon dim goc, roi chon dim hien hanh -> LISP tu tinh L.
;;;    2. Quet 1 hoac nhieu crossing window (nhu STRETCH goc).
;;;       - Go R de doi che do THEM / TRU (window sau se loai doi tuong khoi tap chon).
;;;       - Go L de nhap lai L, go X de lay L = L/2.
;;;       - Enter khi chua quet window nao   -> thoat command.
;;;       - Enter sau khi da quet >= 1 window -> sang buoc stretch.
;;;    3. Click diem dau, roi re chuot va click de chon huong.
;;;       Huong duoc lam tron ve 1 trong 4 truc: X+, X-, Y+, Y- (theo UCS hien hanh).
;;;       Khoang dich chuyen luon dung bang L, khong phu thuoc diem click thu 2.
;;;    4. Sau moi luot, command tu quay lai buoc 2 de lam tiep.
;;;       Enter (khi chua quet window) hoac Esc de ket thuc.
;;;
;;;  Ghi chu ky thuat:
;;;    - Phan stretch cuoi goi thang STRETCH goc cua AutoCAD (voi dung cac crossing
;;;      window da quet) nen hanh vi giong het STRETCH lam tay.
;;;    - OSMODE duoc tat tam thoi de diem click khong bi OSNAP keo lech, va duoc
;;;      khoi phuc khi ket thuc hoac khi Esc.
;;;    - L duoc luu lai giua cac lan chay (registry, key "SSD_LENGTH").
;;;    - Toan bo prompt viet khong dau de khong phu thuoc encoding cua file LISP.
;;;
;;;  Load: APPLOAD hoac keo file vao man hinh AutoCAD.
;;; ============================================================================

(setq *ssd-tol* 1e-6)

;; ---------------------------------------------------------------- format so --
(defun ssd:fmt (v) (rtos (if v v 0.0) 2 3))

;; -------------------------------------------------------- luu / doc L da luu --
(defun ssd:len-get ()
  (if (or (null *ssd-len*) (<= (abs *ssd-len*) *ssd-tol*))
    (setq *ssd-len*
      (cond
        ((and (getenv "SSD_LENGTH") (distof (getenv "SSD_LENGTH") 2))
         (distof (getenv "SSD_LENGTH") 2))
        (T 500.0)
      )
    )
  )
  *ssd-len*
)

(defun ssd:len-set (v)
  (setq *ssd-len* v)
  (setenv "SSD_LENGTH" (rtos v 2 8))
  v
)

;; ------------------------------------------------- luu / khoi phuc sysvar ----
(defun ssd:save-sysvars ()
  (setq *ssd-osmode*  (getvar "OSMODE")
        *ssd-cmdecho* (getvar "CMDECHO")
        *ssd-offscr*  (vl-catch-all-apply 'getvar (list "SELECTIONOFFSCREEN"))
  )
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)
  (if (not (vl-catch-all-error-p *ssd-offscr*))
    (vl-catch-all-apply 'setvar (list "SELECTIONOFFSCREEN" 2))
  )
  (princ)
)

(defun ssd:restore-sysvars ()
  (sssetfirst nil nil)
  (if *ssd-osmode* (setvar "OSMODE" *ssd-osmode*))
  (if (and *ssd-offscr* (not (vl-catch-all-error-p *ssd-offscr*)))
    (vl-catch-all-apply 'setvar (list "SELECTIONOFFSCREEN" *ssd-offscr*))
  )
  (if *ssd-cmdecho* (setvar "CMDECHO" *ssd-cmdecho*))
  (princ)
)

(defun ssd:err (msg)
  (while (> (getvar "CMDACTIVE") 0) (command))
  (ssd:restore-sysvars)
  (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*QUIT*")))
    (princ (strcat "\nSSD - loi: " msg))
    (princ "\nSSD - da ket thuc.")
  )
  (setq *error* *ssd-olderr*)
  (princ)
)

;; ------------------------------------------------------ tap chon: hop / tru --
(defun ssd:ss-add (dest src / i)
  (setq i -1)
  (if src (repeat (sslength src) (ssadd (ssname src (setq i (1+ i))) dest)))
  dest
)

(defun ssd:ss-del (dest src / i n cnt)
  (setq i -1 cnt 0)
  (if src
    (repeat (sslength src)
      (setq n (ssname src (setq i (1+ i))))
      (if (ssmemb n dest) (progn (ssdel n dest) (setq cnt (1+ cnt))))
    )
  )
  cnt
)

;; ------------------------------------------------------ doc measurement DIM --
(defun ssd:dim-measurement (ent / dat typ m obj)
  (setq dat (entget ent)
        typ (cdr (assoc 0 dat))
        m   nil)
  (if (wcmatch typ "*DIMENSION*")
    (progn
      (setq m (cdr (assoc 42 dat)))
      (if (or (null m) (equal m 0.0 *ssd-tol*))
        (progn
          (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
          (if (not (vl-catch-all-error-p obj))
            (progn
              (setq m (vl-catch-all-apply 'vlax-get (list obj 'Measurement)))
              (if (vl-catch-all-error-p m) (setq m nil))
            )
          )
        )
      )
    )
  )
  (if m (abs m))
)

(defun ssd:pick-dim (msg / e m res)
  (setq res nil)
  (while (not res)
    (setq e (entsel msg))
    (cond
      ((null e) (setq res 'cancel))
      ((setq m (ssd:dim-measurement (car e)))
       (if (> m *ssd-tol*)
         (progn
           (princ (strcat "  -> " (ssd:fmt m)))
           (setq res m)
         )
         (princ "\nDim nay co gia tri 0. Hay chon lai.")
       )
      )
      (T (princ "\nDoi tuong khong phai DIM hop le. Hay chon lai."))
    )
  )
  (if (eq res 'cancel) nil res)
)

;; ----------------------------------------------------------- L tu 2 dim ------
(defun ssd:len-from-dims (half / d1 d2 dif len res)
  (setq res nil)
  (while (not res)
    (setq d1 (ssd:pick-dim "\nChon dim goc: "))
    (if (null d1)
      (setq res 'cancel)
      (progn
        (setq d2 (ssd:pick-dim "\nChon dim hien hanh: "))
        (if (null d2)
          (setq res 'cancel)
          (progn
            (setq dif (abs (- d1 d2))
                  len (if half (/ dif 2.0) dif))
            (if (<= len *ssd-tol*)
              (princ "\nHai dim dang cho chenh lech bang 0. Hay chon lai.")
              (progn
                (princ
                  (strcat "\nL = |" (ssd:fmt d1) " - " (ssd:fmt d2) "|"
                          (if half " / 2" "")
                          " = " (ssd:fmt len)))
                (setq res len)
              )
            )
          )
        )
      )
    )
  )
  (if (eq res 'cancel) nil res)
)

;; --------------------------------------------------------- L nhap bang tay ---
(defun ssd:len-prompt (/ v)
  (initget 2)
  (setq v (getreal (strcat "\nNhap L <" (ssd:fmt (ssd:len-get)) ">: ")))
  (cond
    ((null v) (ssd:len-get))
    ((<= (abs v) *ssd-tol*) (princ "\nL khong duoc bang 0.") (ssd:len-get))
    (T (ssd:len-set v) (princ (strcat "\nL = " (ssd:fmt v))) (ssd:len-get))
  )
)

(defun ssd:len-half (label / h)
  (setq h (/ (ssd:len-get) 2.0))
  (if (> (abs h) *ssd-tol*)
    (progn
      (ssd:len-set h)
      (princ (strcat "\n" label ": L = L/2 = " (ssd:fmt h)))
    )
    (princ "\nL/2 khong hop le (L dang bang 0).")
  )
  (ssd:len-get)
)

;; ------------------------------------------- huong + diem dich chuyen theo L --
(defun ssd:dir (p1 p2 / dx dy)
  (setq dx (- (car p2) (car p1))
        dy (- (cadr p2) (cadr p1)))
  (cond
    ((and (< (abs dx) *ssd-tol*) (< (abs dy) *ssd-tol*)) nil)
    ((>= (abs dx) (abs dy)) (if (>= dx 0.0) "X+" "X-"))
    (T (if (>= dy 0.0) "Y+" "Y-"))
  )
)

;; L am -> dao huong (giong FlipDirection ben .NET)
(defun ssd:flip (dir)
  (cond ((= dir "X+") "X-") ((= dir "X-") "X+")
        ((= dir "Y+") "Y-") ((= dir "Y-") "Y+"))
)

(defun ssd:target (base dir len / a d)
  (setq a (abs len)
        d (if (< len 0.0) (ssd:flip dir) dir))
  (cond
    ((= d "X+") (polar base 0.0 a))
    ((= d "X-") (polar base pi a))
    ((= d "Y+") (polar base (/ pi 2.0) a))
    ((= d "Y-") (polar base (* 1.5 pi) a))
    (T base)
  )
)

(defun ssd:dir-label (dir len)
  (if (< len 0.0) (strcat (ssd:flip dir) " (L am nen dao huong)") dir)
)

;; ------------------------------------------------ quet cac crossing window ---
;; Tra ve: 'stop (thoat command) | nil (lam lai) | list cac (mode p1 p2)
(defun ssd:pick-windows (label / wins mode all ss pr p1 p2 res removed)
  (setq wins '() mode "A" all (ssadd) res nil)
  (princ (strcat "\n" label
    ": quet crossing window (nhieu vung neu can). R = doi che do THEM/TRU."))
  (while (not res)
    (initget "L X R")
    (setq pr (getpoint
      (strcat "\n[" (if (= mode "R") "TRU" "THEM") "] Goc dau crossing window"
              (if wins " tiep theo" "")
              " hoac [L/X/R] <L=" (ssd:fmt (ssd:len-get)) ">, Enter de "
              (if wins "stretch" "thoat") ": ")))
    (cond
      ((and (= (type pr) 'STR) (= pr "L")) (ssd:len-prompt))
      ((and (= (type pr) 'STR) (= pr "X")) (ssd:len-half label))
      ((and (= (type pr) 'STR) (= pr "R"))
       (setq mode (if (= mode "R") "A" "R"))
       (princ (strcat "\nChe do window: "
                      (if (= mode "R") "TRU khoi tap chon" "THEM vao tap chon")))
      )
      ((null pr)
       (if wins (setq res (reverse wins)) (setq res 'stop))
      )
      (T
        (setq p1 pr
              p2 (getcorner p1 "\nGoc doi dien crossing window: "))
        (if (null p2)
          (princ "\nBo qua window nay.")
          (progn
            (sssetfirst nil nil)
            (setq ss (ssget "_C" p1 p2))
            (cond
              ((null ss) (princ "\nWindow nay khong bat duoc doi tuong nao."))
              ((= mode "R")
               (setq removed (ssd:ss-del all ss))
               (if (= removed 0)
                 (princ "\nWindow TRU nay khong loai duoc doi tuong nao trong tap chon.")
                 (progn
                   (setq wins (cons (list mode p1 p2) wins))
                   (princ (strcat "\nDa loai " (itoa removed)
                                  " doi tuong. Con lai " (itoa (sslength all)) "."))
                 )
               )
              )
              (T
               (ssd:ss-add all ss)
               (setq wins (cons (list mode p1 p2) wins))
               (princ (strcat "\nDa gom " (itoa (sslength all)) " doi tuong."))
              )
            )
            (if (> (sslength all) 0) (sssetfirst nil all))
          )
        )
      )
    )
  )
  (sssetfirst nil nil)
  (cond
    ((eq res 'stop) 'stop)
    ((= (sslength all) 0) (princ "\nChua co doi tuong nao duoc chon.") nil)
    (T res)
  )
)

;; -------------------------------------------------------------- diem dau ----
;; Tra ve: 'stop | diem
(defun ssd:pick-base (label / pr res)
  (setq res nil)
  (while (not res)
    (initget "L X")
    (setq pr (getpoint
      (strcat "\n" label ": chon diem dau hoac [L/X] <L="
              (ssd:fmt (ssd:len-get)) ">, Enter de ket thuc: ")))
    (cond
      ((and (= (type pr) 'STR) (= pr "L")) (ssd:len-prompt))
      ((and (= (type pr) 'STR) (= pr "X")) (ssd:len-half label))
      ((null pr) (setq res 'stop))
      (T (setq res pr))
    )
  )
  res
)

;; -------------------------------------------- goi STRETCH goc cua AutoCAD ---
(defun ssd:native-stretch (wins base target / cur pts lo hi pad vctr vsz)
  ;; zoom tam vao vung lam viec de AutoCAD khong bo sot doi tuong ngoai man hinh
  (setq pts (list base target))
  (foreach w wins (setq pts (cons (cadr w) (cons (caddr w) pts))))
  (setq lo (list (apply 'min (mapcar 'car pts)) (apply 'min (mapcar 'cadr pts)))
        hi (list (apply 'max (mapcar 'car pts)) (apply 'max (mapcar 'cadr pts))))
  (setq pad (max 10.0 (* 0.15 (max (- (car hi) (car lo)) (- (cadr hi) (cadr lo))))))
  (setq lo (mapcar '- lo (list pad pad))
        hi (mapcar '+ hi (list pad pad)))
  (setq vctr (getvar "VIEWCTR") vsz (getvar "VIEWSIZE"))

  (command "_.ZOOM" "_W" lo hi)

  (command "_.STRETCH")
  (setq cur "A")
  (foreach w wins
    (if (/= (car w) cur)
      (progn
        (command (if (= (car w) "R") "_R" "_A"))
        (setq cur (car w))
      )
    )
    (command "_C" (cadr w) (caddr w))
  )
  (if (> (getvar "CMDACTIVE") 0) (command ""))
  (if (> (getvar "CMDACTIVE") 0) (command base))
  (if (> (getvar "CMDACTIVE") 0) (command target))
  (while (> (getvar "CMDACTIVE") 0) (command ""))

  (command "_.ZOOM" "_C" vctr vsz)
  (princ)
)

;; ---------------------------------------------------------------- 1 luot ----
;; Tra ve: 'stop | 'done | 'retry
(defun ssd:once (label / wins base p2 dir target len)
  (setq wins (ssd:pick-windows label))
  (cond
    ((eq wins 'stop) 'stop)
    ((null wins) 'retry)
    (T
      (setq base (ssd:pick-base label))
      (if (eq base 'stop)
        'stop
        (progn
          (setq len (ssd:len-get))
          (setq p2 (getpoint base
            (strcat "\nRe chuot roi click de chon huong stretch (L = "
                    (ssd:fmt len) "): ")))
          (cond
            ((null p2) (princ "\nChua chon duoc huong. Lam lai.") 'retry)
            ((null (setq dir (ssd:dir base p2)))
             (princ "\nKhong xac dinh duoc huong stretch. Lam lai.") 'retry)
            (T
             (setq target (ssd:target base dir len))
             (ssd:native-stretch wins base target)
             (princ (strcat "\n" label ": da goi STRETCH goc theo "
                            (ssd:dir-label dir len)
                            " voi L = " (ssd:fmt (abs len)) "."))
             'done
            )
          )
        )
      )
    )
  )
)

;; ---------------------------------------------------------- vong lap chinh --
(defun ssd:run (label npass / stop pass r)
  (setq *ssd-olderr* *error*
        *error*      ssd:err)
  (ssd:save-sysvars)
  (setq stop nil)
  (while (not stop)
    (setq pass 1)
    (while (and (not stop) (<= pass npass))
      (if (> npass 1)
        (princ (strcat "\n--- " label ": luot " (itoa pass) "/" (itoa npass)
                       " voi L = " (ssd:fmt (ssd:len-get)) " ---")))
      (setq r (ssd:once (if (> npass 1)
                          (strcat label " [" (itoa pass) "/" (itoa npass) "]")
                          label)))
      (cond
        ((eq r 'stop) (setq stop T))
        ((eq r 'done) (setq pass (1+ pass)))
        (T nil) ; retry: lam lai chinh luot nay
      )
    )
  )
  (ssd:restore-sysvars)
  (setq *error* *ssd-olderr*)
  (princ)
)

;; ================================= COMMANDS ==================================

;; SSD : L = |DIM1 - DIM2|, stretch 1 luot
(defun C:SSD (/ len)
  (ssd:len-get)
  (setq len (ssd:len-from-dims nil))
  (if len
    (progn (ssd:len-set len) (ssd:run "SSD" 1))
    (princ "\nSSD - da huy.")
  )
  (princ)
)

;; SSD2 : L = |DIM1 - DIM2| / 2, stretch 2 luot
(defun C:SSD2 (/ len)
  (ssd:len-get)
  (setq len (ssd:len-from-dims T))
  (if len
    (progn (ssd:len-set len) (ssd:run "SSD2" 2))
    (princ "\nSSD2 - da huy.")
  )
  (princ)
)

;; SSDL : nhap L bang tay 
(defun C:SS-SMART-STRETCH ()
  (ssd:len-get)
  (ssd:len-prompt)
  (if (> (abs (ssd:len-get)) *ssd-tol*)
    (ssd:run "SSDL" 1)
    (princ "\nSSDL - L khong hop le.")
  )
  (princ)
)

(princ "\nSSD(STRETCHBYDIM).LSP da load. Commands: SSD | SSD2 | SSDL")
(princ)
