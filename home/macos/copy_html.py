#!/usr/bin/env python
import pasteboard
import sys

pb = pasteboard.Pasteboard()
pb.set_contents(sys.stdin.read(), type=pasteboard.HTML)
