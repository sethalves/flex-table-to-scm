#! /bin/sh
#| -*- scheme -*-
exec csi -include-path /usr/local/share/scheme -s $0 "$@"
|#

(use r7rs)
(include "snow/bytevector.sld")
(include "seth/port-extras.sld")
(include "seth/binary-pack.sld")
(include "seth/string-read-write.sld")
(include "seth/cout.sld")
(include "seth/flex.sld")
(include "flex-table-to-scm.sld")

(import (scheme base)
        (flex-table-to-scm))
(main-program)
