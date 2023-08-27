## Example Config

A complete example configuration could look as follows.

##### config.toml
```toml
[config]
eww-window = "notification-frame"
eww-content-key = "end-notifications"
eww-default-notification-key = "end-notification"
max-notifications = 10
notification-orientation = "v"

timeout.urgency.low = 5 
timeout.urgency.normal = 10
timeout.urgency.critical = 0

[[notification-type]]
name = "battery-warning"
eww-key = "battery-widget" 
hint = "battery"
timeout = 0
```

##### eww.yuck
```yuck
(defvar end-notifications "")

(defwindow notification_frame
 :monitor 0
 :geometry (geometry 
     :x "0px"
     :y "0px"
     :width "100px"
     :height "100%"
     :anchor "top right")
 :stacking "fg"
 :windowtype "desktop"
 :wm-ignore true
 (literal :content end-notifications))

(defwidget end-notification 
 [end-id end-appname end-appicon end-summary end-body end-hints]
 (box 
  :class "end-default-notification-box"
  (literal :content 
    { end-body != "" 
      ? '
        (box
          :orientation "vertical"
          (label
            :class "notification-title notification-text"    
            :yalign 1
            :xalign 0 
            :text "${end-summary}")
          (label
            :class "notification-content notification-text"
            :yalign 1
            :xalign 0
            :text "${end-body}"))'
      : '
        (label
          :class "notification-title notification-text"    
          :yalign 1
          :xalign 0 
          :text "${end-summary}")'
    })))
```

##### eww.scss
```scss
.end-default-notification-box {
  background-color: $end-default-bg;
  padding: 12px;
  margin: 12px;
  border-radius: 10px;
}

.notification-text {
  color: $end-default-fg;
  font-family: 'FantasqueSansMonoNerdFontAAE Nerd Font Mono'
}

.notification-title {
  font-weight: bold;
  font-size: 1em;
}

.notification-content {
  font-size: .8em;
}
```
