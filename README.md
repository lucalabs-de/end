# Eww Notification Daemon

There are a few great notification daemons around. Unfortunately, they all use their own configuration language. 
If you already use eww for your widgets, wouldn't it be great if you could use it for your notifications as well?

## Features

## Todo
- Add configuration file
- Add different types of notifications that can be chosen via hints (mapping of hints to notification types can be chosen in config file)
- Add 

## Getting Started

## Example Configuration
#### eww.yuck
```
(defvar end-notifications "")
(defvar end-appname "")
(defvar end-appicon "")
(defvar end-summary "")
(defvar end-body "")


(defwindow notification_frame
            :monitor 0
            :geometry (geometry :x "0px"
                                :y "0px"
                                :width "100px"
                                :height "100%"
                                :anchor "top right")
            :stacking "fg"
            :windowtype "desktop"
            :wm-ignore true
            (literal :content end-notifications))

```
