dxmenu : dialog {
  label = "DungX Command Launcher";
  initial_focus = "cmds";

  : row {
    : boxed_column {
      label = "Command List";
      width = 64;

      : popup_list {
        key = "group";
        label = "Source";
        width = 28;
      }

      : list_box {
        key = "cmds";
        width = 64;
        height = 22;
        fixed_width_font = true;
        allow_accept = true;
      }
    }

    : boxed_column {
      label = "Details";
      width = 42;

      : text {
        key = "info_cmd";
        label = "Command:";
        width = 40;
      }

      : text {
        key = "info_src";
        label = "Source:";
        width = 40;
      }

      : text {
        key = "info_desc";
        label = "Mo ta:";
        width = 40;
      }

      : spacer {
        height = 1;
      }

      : text {
        key = "status";
        label = "Trang thai: san sang";
        width = 40;
      }

      : spacer {
        height = 1;
      }

      : text {
        label = "Double-click de chay lenh.";
        width = 40;
      }

      : text {
        label = "Reload sau khi sua 2 file goc.";
        width = 40;
      }
    }
  }

  : row {
    fixed_width = true;

    : button {
      key = "run";
      label = "Run";
      is_default = true;
      width = 12;
    }

    : button {
      key = "reload";
      label = "Reload";
      width = 12;
    }

    : button {
      key = "cancel";
      label = "Close";
      is_cancel = true;
      width = 12;
    }
  }
}
