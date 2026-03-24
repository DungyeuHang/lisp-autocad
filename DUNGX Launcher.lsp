(vl-load-com)

(defun dxmenu-env-root ()
  (getenv "DUNGX_LAUNCHER_ROOT")
)

(defun dxmenu-startup-dir (/ guess)
  (setq guess
    (cond
      ((dxmenu-env-root))
      (*load-truename* *load-truename*)
      (*load-pathname* *load-pathname*)
      ((findfile "DUNGX Launcher.lsp"))
    )
  )
  (if guess
    (vl-filename-directory guess)
    (getvar "DWGPREFIX")
  )
)

(setq *dxmenu-root* (dxmenu-startup-dir))

(defun dxmenu-save-root (root)
  (if (and root (/= root ""))
    (progn
      (setq *dxmenu-root* root)
      (setenv "DUNGX_LAUNCHER_ROOT" root)
    )
  )
  *dxmenu-root*
)

(defun dxmenu-path (fileName / root)
  (setq root *dxmenu-root*)
  (if (or (null root) (= root ""))
    fileName
    (if (= (substr root (strlen root) 1) "\\")
      (strcat root fileName)
      (strcat root "\\" fileName)
    )
  )
)

(defun dxmenu-find-file (fileName / resolved)
  (setq resolved
    (cond
      ((findfile fileName))
    )
  )
  resolved
)

(defun dxmenu-resolve-file (fileName / candidate)
  (setq candidate (dxmenu-path fileName))
  (cond
    ((dxmenu-find-file candidate))
    ((dxmenu-find-file fileName))
    (T nil)
  )
)

(defun dxmenu-pick-root (/ picked)
  (setq picked
    (getfiled
      "Chon file DUNGX Launcher.dcl"
      (if (and *dxmenu-root* (/= *dxmenu-root* ""))
        *dxmenu-root*
        (getvar "DWGPREFIX")
      )
      "dcl"
      8
    )
  )
  (if picked
    (dxmenu-save-root (vl-filename-directory picked))
  )
)

(defun dxmenu-ensure-root (/ ok)
  (setq ok
    (and
      (dxmenu-resolve-file "DUNGX Launcher.dcl")
      (dxmenu-resolve-file "DUNGX Custom Command.LSP")
      (dxmenu-resolve-file "DUNGX 2.LSP")
    )
  )
  (if ok
    T
    (progn
      (prompt "\nKhong tim thay bo file launcher. Hay chon file DUNGX Launcher.dcl trong thu muc dang dung.")
      (if (dxmenu-pick-root)
        (and
          (dxmenu-resolve-file "DUNGX Launcher.dcl")
          (dxmenu-resolve-file "DUNGX Custom Command.LSP")
          (dxmenu-resolve-file "DUNGX 2.LSP")
        )
        nil
      )
    )
  )
)

(defun dxmenu-source-files ()
  '("DUNGX Custom Command.LSP" "DUNGX 2.LSP")
)

(defun dxmenu-command-data ()
  '(
    ("1" "Line + set layer _mss.bao" "DUNGX Custom")
    ("2" "Move with full osnap" "DUNGX Custom")
    ("4" "Layer on" "DUNGX Custom")
    ("5" "Layer off" "DUNGX Custom")
    ("55" "Mocoro" "DUNGX Custom")
    ("44" "Matchprop" "DUNGX Custom")
    ("CC" "Copy and save new objects to P" "DUNGX Custom")
    ("qq" "Quick Select" "DUNGX Custom")
    ("ff" "Filter" "DUNGX Custom")
    ("dd" "Dimlinear" "DUNGX Custom")
    ("f1" "Find" "DUNGX Custom")
    ("dss" "Dimspace" "DUNGX Custom")
    ("dcc" "Dimcontinue" "DUNGX Custom")
    ("c1" "Center + midpoint osnap mode" "DUNGX Custom")
    ("CES" "Copy to midpoint and erase source" "DUNGX Custom")
    ("DUNGX" "Auto insert SIGNATURE by frame" "DUNGX Custom")
    ("kcs3" "Keo cua so 3 canh" "DUNGX Custom")
    ("ooo" "Tat 5 layer de lam phoi" "DUNGX Custom")
    ("ll" "Last selection" "DUNGX Custom")
    ("sx" "Stretch X" "DUNGX Custom")
    ("SY" "Stretch Y" "DUNGX Custom")
    ("vls" "Ve lo song" "DUNGX Custom")
    ("vss" "Ve song 3 kieu" "DUNGX Custom")
    ("DFL025" "Set dim scale 0.25" "DUNGX Custom")
    ("DFL1" "Set dim scale 1" "DUNGX Custom")
    ("CCFX" "Copy and fit X" "DUNGX Custom")
    ("vvd" "Ve va danh diem" "DUNGX Custom")
    ("APOINT" "Make points from polyline" "DUNGX Custom")
    ("RTPL" "Doi diem dau polyline" "DUNGX Custom")
    ("v2v" "Viewports 2 vertical" "DUNGX Custom")
    ("v2h" "Viewports 2 horizontal" "DUNGX Custom")
    ("1v" "Single viewport" "DUNGX Custom")
    ("JoinDimAuto" "Gop nhieu dim thanh 1 dim" "DUNGX Custom")
    ("UFF" "Bo fillet polyline" "DUNGX Custom")
    ("vaa-ve-arc" "Vault / ve arc tu kich thuoc" "DUNGX Custom")
    ("CTC" "Copy from center to circle centers" "DUNGX Custom")
    ("IPP-Insert-PG" "Insert phao gia" "DUNGX Custom")
    ("IPS-Insert-PGS" "Insert phao gia series" "DUNGX Custom")
    ("DIMBN" "Dim 4 ben theo bbox + polyline" "DUNGX Custom")
    ("33" "Mirror and save new set to P" "DUNGX Custom")
    ("CMMM" "Copy theo cap line/polyline" "DUNGX Custom")
    ("350" "Xu ly khoa KMD-350" "DUNGX 2")
    ("535" "Xu ly khoa KMD-535" "DUNGX 2")
    ("TKKTHAYKHOA" "Thay khoa cu bang khoa moi" "DUNGX 2")
    ("SAY_STRETCH_BY_DIM" "Stretch theo dim doc" "DUNGX 2")
    ("SAX_STRETCH_BY_DIM" "Stretch theo dim ngang" "DUNGX 2")
    ("DCS" "Dung chi tiet bao + mirror" "DUNGX 2")
    ("del_a$" "Purge block A$*" "DUNGX 2")
  )
)

(defun dxmenu-groups ()
  '("Tat ca" "DUNGX Custom" "DUNGX 2")
)

(defun dxmenu-nth (items index / count result)
  (setq count 0)
  (setq result nil)
  (while (and items (null result))
    (if (= count index)
      (setq result (car items))
      (progn
        (setq count (1+ count))
        (setq items (cdr items))
      )
    )
  )
  result
)

(defun dxmenu-trim (text maxLen)
  (if (> (strlen text) maxLen)
    (strcat (substr text 1 (- maxLen 3)) "...")
    text
  )
)

(defun dxmenu-display-line (item)
  (strcat
    (dxmenu-trim (car item) 16)
    " | "
    (dxmenu-trim (cadr item) 34)
    " | "
    (dxmenu-trim (caddr item) 12)
  )
)

(defun dxmenu-filtered-items (group / allItems item result)
  (setq allItems (dxmenu-command-data))
  (setq result '())
  (foreach item allItems
    (if (or (= group "Tat ca") (= (caddr item) group))
      (setq result (append result (list item)))
    )
  )
  result
)

(defun dxmenu-selected-item ()
  (if (and *dxmenu-items*
           (numberp *dxmenu-selected-index*)
           (>= *dxmenu-selected-index* 0)
           (< *dxmenu-selected-index* (length *dxmenu-items*)))
    (dxmenu-nth *dxmenu-items* *dxmenu-selected-index*)
  )
)

(defun dxmenu-set-status (message)
  (set_tile "status" (strcat "Trang thai: " (dxmenu-trim message 28)))
)

(defun dxmenu-update-info (/ item)
  (setq item (dxmenu-selected-item))
  (if item
    (progn
      (set_tile "info_cmd" (strcat "Command: " (car item)))
      (set_tile "info_src" (strcat "Source: " (caddr item)))
      (set_tile "info_desc" (strcat "Mo ta: " (dxmenu-trim (cadr item) 28)))
    )
    (progn
      (set_tile "info_cmd" "Command:")
      (set_tile "info_src" "Source:")
      (set_tile "info_desc" "Mo ta:")
    )
  )
)

(defun dxmenu-fill-list (group / item)
  (setq *dxmenu-items* (dxmenu-filtered-items group))
  (start_list "cmds")
  (foreach item *dxmenu-items*
    (add_list (dxmenu-display-line item))
  )
  (end_list)

  (if *dxmenu-items*
    (progn
      (setq *dxmenu-selected-index* 0)
      (set_tile "cmds" "0")
    )
    (setq *dxmenu-selected-index* nil)
  )

  (dxmenu-update-info)
)

(defun dxmenu-current-group ()
  (dxmenu-nth *dxmenu-groups* (atoi (get_tile "group")))
)

(defun dxmenu-load-sources (/ filePath fileName loaded missing)
  (setq loaded 0)
  (setq missing 0)
  (foreach fileName (dxmenu-source-files)
    (setq filePath (dxmenu-resolve-file fileName))
    (if filePath
      (progn
        (load filePath nil)
        (setq loaded (1+ loaded))
      )
      (setq missing (1+ missing))
    )
  )
  (if (> missing 0)
    (strcat "reload " (itoa loaded) "/" (itoa (+ loaded missing)) ", thieu " (itoa missing) " file")
    (strcat "reload " (itoa loaded) "/" (itoa loaded) " file")
  )
)

(defun dxmenu-on-group-change (value / groupName)
  (setq groupName (dxmenu-nth *dxmenu-groups* (atoi value)))
  (dxmenu-fill-list groupName)
)

(defun dxmenu-on-command-change (value)
  (setq *dxmenu-selected-index* (atoi value))
  (dxmenu-update-info)
)

(defun dxmenu-do-reload ()
  (dxmenu-set-status (dxmenu-load-sources))
  (dxmenu-fill-list (dxmenu-current-group))
)

(defun dxmenu-run-selected-command (/ item commandName)
  (setq item (dxmenu-selected-item))
  (if item
    (progn
      (dxmenu-load-sources)
      (setq commandName (car item))
      (prompt (strcat "\nDang chay command: " commandName))
      (vl-cmdf commandName)
    )
    (prompt "\nChua chon command.")
  )
  (princ)
)

(defun dxmenu-open (/ dclId dclPath dialogResult)
  (if (not (dxmenu-ensure-root))
    (alert "Khong tim thay day du DCL/LSP trong thu muc da chon.")
    (progn
      (setq dclPath (dxmenu-resolve-file "DUNGX Launcher.dcl"))
      (setq *dxmenu-groups* (dxmenu-groups))
      (setq *dxmenu-items* nil)
      (setq *dxmenu-selected-index* 0)
      (setq *dxmenu-dialog-result* nil)

      (dxmenu-load-sources)

      (setq dclId (load_dialog dclPath))
      (if (or (< dclId 0) (not (new_dialog "dxmenu" dclId)))
        (alert "Khong mo duoc DungX Launcher dialog.")
        (progn
          (start_list "group")
          (foreach dialogResult *dxmenu-groups*
            (add_list dialogResult)
          )
          (end_list)

          (set_tile "group" "0")
          (dxmenu-fill-list "Tat ca")
          (dxmenu-set-status "san sang")

          (action_tile "group" "(dxmenu-on-group-change $value)")
          (action_tile "cmds" "(dxmenu-on-command-change $value)")
          (action_tile "reload" "(dxmenu-do-reload)")
          (action_tile "run" "(setq *dxmenu-dialog-result* T)(done_dialog 1)")
          (action_tile "cancel" "(setq *dxmenu-dialog-result* nil)(done_dialog 0)")

          (setq dialogResult (start_dialog))
        )
      )

      (if (>= dclId 0)
        (unload_dialog dclId)
      )

      (if (and (= dialogResult 1) *dxmenu-dialog-result*)
        (dxmenu-run-selected-command)
      )
    )
  )
  (princ)
)

(defun c:DXMENU ()
  (dxmenu-open)
)

(defun c:DUNGXBANG ()
  (dxmenu-open)
)

(defun c:DXMENUCHECK (/ dclPath customPath dungx2Path)
  (if (not (dxmenu-resolve-file "DUNGX Launcher.dcl"))
    (prompt "\nRoot hien tai chua dung. Neu muon chon lai, go DXMENUSETROOT.")
  )
  (setq dclPath (dxmenu-resolve-file "DUNGX Launcher.dcl"))
  (setq customPath (dxmenu-resolve-file "DUNGX Custom Command.LSP"))
  (setq dungx2Path (dxmenu-resolve-file "DUNGX 2.LSP"))

  (prompt (strcat "\nLauncher root: " (if *dxmenu-root* *dxmenu-root* "<nil>")))
  (prompt (strcat "\nDCL: " (if dclPath dclPath "<khong tim thay>")))
  (prompt (strcat "\nDUNGX Custom: " (if customPath customPath "<khong tim thay>")))
  (prompt (strcat "\nDUNGX 2: " (if dungx2Path dungx2Path "<khong tim thay>")))
  (princ)
)

(defun c:DXMENUSETROOT ()
  (if (dxmenu-pick-root)
    (prompt (strcat "\nDa luu launcher root: " *dxmenu-root*))
    (prompt "\nKhong thay doi launcher root.")
  )
  (princ)
)

(princ "\nDungX Launcher loaded. Run DXMENU, DUNGXBANG, DXMENUCHECK, or DXMENUSETROOT.")
(princ)
