# Eww Notification Daemon

There are a few great notification daemons around. Unfortunately, they all use their own configuration language. 
If you already use eww for your widgets, wouldn't it be great if you could just use it for your notifications as well?

## Features
The Eww Notification Daemon, or End for short, allows you to do exactly that. It leverages eww's `literal` widget to dynamically display 
libnotify notifications. You define the eww widgets that are used to display a notification and the eww window that they will appear in. You can 
define multiple widgets for your notifications and select between them via libnotify hints. 


## Getting Started
### Starting End
You can either build the project yourself or use one of the prebuilt binaries under Releases. To start the notification daemon, simply run the executable. You'll probably want to put something like
```bash
end &
```
in your `.xinitrc` or similar.

### Eww Configuration
You need to provide an eww window that End will use to show notifications. For this to work, the window is required 
to contain the widget `(literal :content end-notifications)`, where `end-notifications` is an eww variable that needs to be defined using `(defvar end-notifications "")`.
The name of the variable is configurable.

## Configuration
End checks `$XDG_CONFIG_HOME/end` for a `config.toml`, which is structured as follows.

```toml
[config]
### Eww variable name used for the "literal" widget.
eww-content-key = "end-notifications"

### Name of the widget used for general notifications. If this is not supplied, End will 
### fall back to a default widget (which is really ugly, so you'll want to set this ^^).
eww-default-notification-key = ""

### The maximal number of notifications that are shown at a time. When the current number of 
### visible notifications exceeds this value, the notification with the soonest timeout will be 
### dropped. If none of the notifications have a timeout, the oldest will be dropped.
###
### A value of 0 means that notifications will never get dropped.
max-notifications = 0

### Defines the timeouts for different types of notifications in seconds. The default timeout
### is used when the notification contains no urgency information.
timeout.default = 5
timeout.urgency.low = 3 
timeout.urgency.normal = 5
timeout.urgency.critical = 0

### This allows you to define custom notification types for special purposes. You can define as many as you want.
[[notification-type]]
### The name of the notification. 
name = "battery-warning"

### The name of the eww widget that should be used to display this type of notification.
eww-key = "battery_widget" 

### The hint value for the key "end-type" that triggers this type of notification. This 
### example notification could, for instance, be triggered by running 
### "notify-send --hint=string:end-type:battery 'battery low'".
hint = "battery"

```
### Custom Notification Widgets
We've seen that you can define your own notification widgets. 
The notification data is supplied to the widget via the parameters `end-appname` `end-appicon` `end-summary` `end-body` and `end-hints`.
As such, a general notification widget looks as follows.

```lisp
(defwidget end-notification 
 [end-appname end-appicon end-summary end-body end-hints]
 ... your content ...)
```

The parameters correspond to the libnotify [notification components](https://specifications.freedesktop.org/notification-spec/notification-spec-latest.html#basic-design).

#### Remark: End-Hints 
In the future, I plan to parse the libnotify standard hints (like `image-path`, `category`, etc.) internally and expose the corresponding values
individually as eww widget parameters. Until then you will need to parse them from the `end-hints` parameter manually using eww's expressions.

### Example Configuration
A complete example configuration could look as follows.

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

 ...

```
#### config.toml
```toml
...
```

## Todo
- Add configuration file
- Add different types of notifications that can be chosen via hints (mapping of hints to notification types can be chosen in config file)
- Support libnotify standard hints
- Finish example configuration
