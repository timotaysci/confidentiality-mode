!(./conf-mode.jpg)

# Confidentiality mode

 This mode is written after several long train rides where I wanted to avoid people looking over my shoulder. I wasn't working on anything confidential but the thought of prying eyes made me feel uncomfortable. I mean, think of all the excellent ideas I may have jotted down that might have been stolen!

 This mode will randomise all text on the screen and only reveal the text in the line/sentence the cursor is on.

 Type away you crazy horses.

 Feedback and comments very much welcome.

## Installation

Clone the repo and add the following to your config:

### Without Melpa
Add the below to your configuration:

`(add-to-list 'load-path "/path/to/folder")`


`(require 'confidentiality-mode)`

### With Melpa
Run the following commands to install

`M-x install-package RET`
`confidentiality-mode RET`

Then add the following to your config

```elisp
(require 'confidentiality-mode)
```


## Usage

Simply envoke the mode to apply it to a buffer.

`M-x confidentiality-mode RET`

