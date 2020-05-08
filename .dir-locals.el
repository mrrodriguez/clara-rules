;;; Without this, `cider` will always prompt for which build name to use. This project only has one
;;; useful dev-time build, so it is set here. See
;;; https://docs.cider.mx/cider/config/project_config.html
((nil
  (cider-figwheel-main-default-options . "dev")))
