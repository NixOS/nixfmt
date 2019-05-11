/*
Copyright Serokell OU <hi@serokell.io>
SPDX-License-Identifier: MPL-2.0
*/

function nixfmt(text, width=80, filename="<stdin>") {
    const param = {width, filename}
    nixfmt_(text, param)
    return {text: param.ret, err: param.err}
}
