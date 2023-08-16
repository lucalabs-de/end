# Eww Notification Daemon

There are a few great notification daemons around. Unfortunately, they all use their own configuration language. 
If you already use eww for your widgets, wouldn't it be great if you could just use it for your notifications as well?

## Features
The Eww Notification Daemon, or End for short, allows you to do exactly that. It leverages eww's `literal` widget to dynamically display 
libnotify notifications. You define the eww widgets that are used to display a notification and the eww window that they will appear in. You can 
define different widgets for notifications that are selected via libnotify hints. 


## Getting Started
### Starting End
You can either build the project yourself or use one of the prebuilt binaries under Releases. To start the notification daemon, simply run the executable. You'll probably want to put something like
```bash
end &
```
in your `.xinitrc` or similar.

### Eww Configuration
You need to provide an eww window that End will use to show notifications. End makes use of eww's `literal` widget. Therefore the window is required 
to contain the widget `(literal :content end-notifications)`, where `end-notifications` is an eww variable that needs to be defined using `(defvar end-notifications "")`.

## Configuration
End checks `$XDG_CONFIG_HOME/end` for a `config.toml`, which is structured as follows

```toml
[config]
eww-content-key = "end-notificatiosn"

[[notification-type]]
name = ""
eww-key = "" 

```

### Example Configuration
#### eww.yuck
```lisp
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

```

## Todo
    - Add configuration file
- Add different types of notifications that can be chosen via hints (mapping of hints to notification types can be chosen in config file)
