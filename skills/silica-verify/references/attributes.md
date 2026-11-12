# Attribute values in mlx

Generated from the attribute tables of the ppx (`ppx/html.ml`). An attribute that is not listed here takes a `string`. An attribute with variants takes one of them, such as ``type_=`number``; a string such as `type_="number"` does not compile. An optional attribute, written `?name`, takes an option of the same type.

| Attribute | Elements | Value |
|---|---|---|
| `accumulate` | every SVG element | `` `none ``, `` `sum `` |
| `additive` | every SVG element | `` `replace ``, `` `sum `` |
| `align` | `td`, `th` | `` `left ``, `` `center ``, `` `right ``, `` `justify ``, `` `char `` |
| `alignmentBaseline` | every SVG element | `` `auto ``, `` `baseline ``, `` `before_edge `` ("before-edge"), `` `text_before_edge `` ("text-before-edge"), `` `middle ``, `` `central ``, `` `after_edge `` ("after-edge"), `` `text_after_edge `` ("text-after-edge"), `` `ideographic ``, `` `alphabetic ``, `` `hanging ``, `` `mathematical ``, `` `inherit_ `` ("inherit") |
| `allowPopups` | `webview` | `bool` |
| `allowReorder` | every SVG element | `` `no ``, `` `yes `` |
| `allowfullcreen` | `webview` | `bool` |
| `allowfullscreen` | `iframe` | `bool` |
| `arabicForm` | every SVG element | `` `initial ``, `` `medial ``, `` `terminal ``, `` `isolated `` |
| `aria_atomic` | every HTML element, every SVG element | `bool`, written as "true" or "false" |
| `aria_autocomplete` | every HTML element, every SVG element | `` `none ``, `` `inline ``, `` `list ``, `` `both `` |
| `aria_busy` | every HTML element, every SVG element | `bool`, written as "true" or "false" |
| `aria_checked` | every HTML element, every SVG element | `` `false_ `` ("false"), `` `mixed ``, `` `true_ `` ("true") |
| `aria_colcount` | every HTML element, every SVG element | `int` |
| `aria_colindex` | every HTML element, every SVG element | `int` |
| `aria_colspan` | every HTML element, every SVG element | `int` |
| `aria_current` | every HTML element, every SVG element | `` `false_ `` ("false"), `` `true_ `` ("true"), `` `page ``, `` `step ``, `` `location ``, `` `date ``, `` `time `` |
| `aria_disabled` | every HTML element, every SVG element | `bool`, written as "true" or "false" |
| `aria_expanded` | every HTML element, every SVG element | `bool`, written as "true" or "false" |
| `aria_haspopup` | every HTML element, every SVG element | `` `false_ `` ("false"), `` `true_ `` ("true"), `` `menu ``, `` `listbox ``, `` `tree ``, `` `grid ``, `` `dialog `` |
| `aria_hidden` | every HTML element, every SVG element | `bool`, written as "true" or "false" |
| `aria_invalid` | every HTML element, every SVG element | `` `false_ `` ("false"), `` `true_ `` ("true"), `` `grammar ``, `` `spelling `` |
| `aria_level` | every HTML element, every SVG element | `int` |
| `aria_live` | every HTML element, every SVG element | `` `off ``, `` `assertive ``, `` `polite `` |
| `aria_modal` | every HTML element, every SVG element | `bool`, written as "true" or "false" |
| `aria_multiline` | every HTML element, every SVG element | `bool`, written as "true" or "false" |
| `aria_multiselectable` | every HTML element, every SVG element | `bool`, written as "true" or "false" |
| `aria_orientation` | every HTML element, every SVG element | `` `horizontal ``, `` `vertical `` |
| `aria_posinset` | every HTML element, every SVG element | `int` |
| `aria_pressed` | every HTML element, every SVG element | `` `false_ `` ("false"), `` `mixed ``, `` `true_ `` ("true") |
| `aria_readonly` | every HTML element, every SVG element | `bool`, written as "true" or "false" |
| `aria_required` | every HTML element, every SVG element | `bool`, written as "true" or "false" |
| `aria_rowcount` | every HTML element, every SVG element | `int` |
| `aria_rowindex` | every HTML element, every SVG element | `int` |
| `aria_rowspan` | every HTML element, every SVG element | `int` |
| `aria_selected` | every HTML element, every SVG element | `bool`, written as "true" or "false" |
| `aria_setsize` | every HTML element, every SVG element | `int` |
| `aria_sort` | every HTML element, every SVG element | `` `none ``, `` `ascending ``, `` `descending ``, `` `other `` |
| `aria_valuemax` | every HTML element, every SVG element | `int` |
| `aria_valuemin` | every HTML element, every SVG element | `int` |
| `aria_valuenow` | every HTML element, every SVG element | `int` |
| `async` | `script` | `bool` |
| `autoReverse` | every SVG element | `bool`, written as "true" or "false" |
| `autoSize` | `webview` | `bool` |
| `autocapitalize` | every HTML element | `` `off ``, `` `none ``, `` `on ``, `` `sentences ``, `` `words ``, `` `characters `` |
| `autofocus` | every HTML element | `bool` |
| `autoplay` | `audio` | `bool` |
| `blocking` | `link`, `script` | `` `render `` |
| `checked` | `input` | `bool` |
| `clipRule` | every SVG element | `` `nonzero ``, `` `evenodd ``, `` `inherit_ `` ("inherit") |
| `cols` | `textarea` | `int` |
| `colspan` | `td`, `th` | `int` |
| `contenteditable` | every HTML element | `bool`, written as "true" or "false" |
| `controls` | `audio` | `bool` |
| `decoding` | `img` | `` `async ``, `` `auto ``, `` `sync `` |
| `default` | `track` | `bool` |
| `defer` | `script` | `bool` |
| `dir` | every HTML element | `` `ltr ``, `` `rtl ``, `` `auto `` |
| `disableGuestResize` | `webview` | `bool` |
| `disableWebSecurity` | `webview` | `bool` |
| `disabled` | `button`, `fieldset`, `input`, `keygen`, `link`, `optgroup`, `option`, `select`, `textarea` | `bool` |
| `disablepictureinpicture` | `video` | `bool` |
| `draggable` | every HTML element | `bool`, written as "true" or "false" |
| `enctype` | `form` | `` `application_x_www_form_urlencoded `` ("application/x-www-form-urlencoded"), `` `multipart_form_data `` ("multipart/form-data"), `` `text_plain `` ("text/plain") |
| `enterkeyhint` | every HTML element | `` `enter ``, `` `done_ `` ("done"), `` `go ``, `` `next ``, `` `previous ``, `` `search ``, `` `send `` |
| `externalResourcesRequired` | every SVG element | `bool`, written as "true" or "false" |
| `fetchpriority` | `img`, `link`, `script` | `` `high ``, `` `low ``, `` `auto `` |
| `fillRule` | every SVG element | `` `nonzero ``, `` `evenodd ``, `` `inherit_ `` ("inherit") |
| `formnovalidate` | `button`, `input` | `bool` |
| `hidden` | every HTML element | `bool` |
| `high` | `meter` | `int` |
| `inert` | every HTML element | `bool` |
| `inputmode` | every HTML element | `` `none ``, `` `text ``, `` `tel ``, `` `url ``, `` `email ``, `` `numeric ``, `` `decimal ``, `` `search `` |
| `ismap` | `img` | `bool` |
| `itemscope` | every HTML element | `bool` |
| `kind` | `track` | `` `subtitles ``, `` `captions ``, `` `descriptions ``, `` `chapters ``, `` `metadata `` |
| `loading` | `iframe`, `img` | `` `lazy_ `` ("lazy"), `` `eager `` |
| `loop` | `audio` | `bool` |
| `low` | `meter` | `int` |
| `marginheight` | `iframe` | `int` |
| `marginwidth` | `iframe` | `int` |
| `maxlength` | `input`, `textarea` | `int` |
| `method_` | `form` | `` `get ``, `` `post ``, `` `dialog `` |
| `minlength` | `input`, `textarea` | `int` |
| `multiple` | `input`, `select` | `bool` |
| `muted` | `audio` | `bool` |
| `nodeIntegration` | `webview` | `bool` |
| `nomodule` | `script` | `bool` |
| `novalidate` | `form` | `bool` |
| `open` | `details`, `dialog` | `bool` |
| `optimum` | `meter` | `int` |
| `overlineThickness` | every SVG element | `int` |
| `playsinline` | `audio`, `video` | `bool` |
| `plugins` | `webview` | `bool` |
| `popover` | every HTML element | `` `auto ``, `` `manual ``, `` `hint `` |
| `popovertargetaction` | `button`, `input` | `` `hide ``, `` `show ``, `` `toggle `` |
| `preload` | `audio` | `` `none ``, `` `metadata ``, `` `auto `` |
| `preserveAlpha` | `feConvolveMatrix` | `bool`, written as "true" or "false" |
| `readonly` | `input`, `textarea` | `bool` |
| `referrerpolicy` | `a`, `area`, `iframe`, `img`, `script` | `` `no_referrer `` ("no-referrer"), `` `no_referrer_when_downgrade `` ("no-referrer-when-downgrade"), `` `origin ``, `` `origin_when_cross_origin `` ("origin-when-cross-origin"), `` `same_origin `` ("same-origin"), `` `strict_origin `` ("strict-origin"), `` `strict_origin_when_cross_origin `` ("strict-origin-when-cross-origin"), `` `unsafe_url `` ("unsafe-url") |
| `required` | `input`, `select`, `textarea` | `bool` |
| `results` | every HTML element | `int` |
| `reversed` | `ol` | `bool` |
| `role` | every HTML element, every SVG element | `` `alert ``, `` `alertdialog ``, `` `application ``, `` `article ``, `` `banner ``, `` `button ``, `` `cell ``, `` `checkbox ``, `` `columnheader ``, `` `combobox ``, `` `complementary ``, `` `contentinfo ``, `` `definition ``, `` `dialog ``, `` `directory ``, `` `document ``, `` `feed ``, `` `figure ``, `` `form ``, `` `grid ``, `` `gridcell ``, `` `group ``, `` `heading ``, `` `img ``, `` `link ``, `` `list ``, `` `listbox ``, `` `listitem ``, `` `log ``, `` `main ``, `` `marquee ``, `` `math ``, `` `menu ``, `` `menubar ``, `` `menuitem ``, `` `menuitemcheckbox ``, `` `menuitemradio ``, `` `navigation ``, `` `none ``, `` `note ``, `` `option ``, `` `presentation ``, `` `progressbar ``, `` `radio ``, `` `radiogroup ``, `` `region ``, `` `row ``, `` `rowgroup ``, `` `rowheader ``, `` `scrollbar ``, `` `search ``, `` `searchbox ``, `` `separator ``, `` `slider ``, `` `spinbutton ``, `` `status ``, `` `switch ``, `` `tab ``, `` `table ``, `` `tablist ``, `` `tabpanel ``, `` `term ``, `` `textbox ``, `` `timer ``, `` `toolbar ``, `` `tooltip ``, `` `tree ``, `` `treegrid ``, `` `treeitem `` |
| `rows` | `textarea` | `int` |
| `rowspan` | `td`, `th` | `int` |
| `scope` | `td`, `th` | `` `row ``, `` `col ``, `` `rowgroup ``, `` `colgroup `` |
| `scoped` | `style` | `bool` |
| `seamless` | `iframe` | `bool` |
| `selected` | `option` | `bool` |
| `size` | `input`, `select` | `int` |
| `span` | `col`, `colgroup` | `int` |
| `spellcheck` | every HTML element | `bool`, written as "true" or "false" |
| `start` | `ol` | `int` |
| `strokeLinecap` | every SVG element | `` `butt ``, `` `round ``, `` `square ``, `` `inherit_ `` ("inherit") |
| `strokeLinejoin` | every SVG element | `` `arcs ``, `` `bevel ``, `` `miter ``, `` `miter_clip `` ("miter-clip"), `` `round `` |
| `tabindex` | every HTML element, every SVG element | `int` |
| `target` | `a`, `area` | `` `_blank ``, `` `_self ``, `` `_parent ``, `` `_top `` |
| `translate` | every HTML element | `` `yes ``, `` `no `` |
| `type_` | `input` | `` `button ``, `` `checkbox ``, `` `color ``, `` `date ``, `` `datetime_local `` ("datetime-local"), `` `email ``, `` `file ``, `` `hidden ``, `` `image ``, `` `month ``, `` `number ``, `` `password ``, `` `radio ``, `` `range ``, `` `reset ``, `` `search ``, `` `submit ``, `` `tel ``, `` `text ``, `` `time ``, `` `url ``, `` `week `` |
| `type_` | `button` | `` `submit ``, `` `reset ``, `` `button `` |
| `valign` | `td` | `` `top ``, `` `middle ``, `` `bottom ``, `` `baseline `` |
| `virtualkeyboardpolicy` | every HTML element | `` `auto ``, `` `manual `` |
| `wrap` | `textarea` | `` `hard ``, `` `soft `` |
| `writingsuggestions` | every HTML element | `` `true_ `` ("true"), `` `false_ `` ("false") |
