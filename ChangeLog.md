# Changelog for dbus-menu

## 0.1.1.1

* Fix "menu already attached" GTK warning by removing redundant
  `menuAttachToWidget` call on submenus (superseded by `menuItemSetSubmenu`)

## 0.1.1.0

* Expand module exports (DBusMenu, DBusMenu.Client, DBusMenu.Client.Util)
* Add `dbusmenu-` CSS style classes to menu widgets for theming
* Fix flaky submenu activation by honoring the `children-display` property

## 0.1.0.0

* Initial release, extracted from gtk-sni-tray
