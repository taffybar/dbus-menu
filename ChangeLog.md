# Changelog for dbus-menu

## 0.1.2.0

* Fix submenu clicks being silently dropped: run `AboutToShow` and `GetLayout`
  DBus calls on a forked thread instead of blocking the GTK main loop. Widget
  updates are posted back via `idleAdd` at `PRIORITY_DEFAULT_IDLE` so pending
  click events are processed first.
* Remove redundant `onMenuItemActivate` refresh trigger on parent items (the
  `onWidgetShow` handler on the submenu is sufficient and avoids a double
  rebuild that created additional windows for lost clicks).

## 0.1.1.1

* Fix "menu already attached" GTK warning by removing redundant
  `menuAttachToWidget` call on submenus (superseded by `menuItemSetSubmenu`)

## 0.1.1.0

* Expand module exports (DBusMenu, DBusMenu.Client, DBusMenu.Client.Util)
* Add `dbusmenu-` CSS style classes to menu widgets for theming
* Fix flaky submenu activation by honoring the `children-display` property

## 0.1.0.0

* Initial release, extracted from gtk-sni-tray
