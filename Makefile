.PHONY: build test smoke check clean

build:
	cabal build

test:
	cabal test all

smoke:
	cabal exec peg-check -- examples/anbn.peg aabb
	! cabal exec peg-check -- examples/anbn.peg aaabb
	cabal exec peg-check -- examples/anbncn.peg aaabbbccc
	! cabal exec peg-check -- examples/anbncn.peg aabbbccc
	cabal exec peg-check -- examples/simple_expression.peg "2+3*4"
	! cabal exec peg-check -- examples/simple_expression.peg "2+"
	cabal exec peg-check -- examples/identifier.peg user_1
	! cabal exec peg-check -- examples/identifier.peg if
	cabal exec peg-check -- examples/number.peg -3.14
	! cabal exec peg-check -- examples/number.peg 3.
	cabal exec peg-check -- examples/hex_color.peg "#ff00AA"
	! cabal exec peg-check -- examples/hex_color.peg "#abcd"
	cabal exec peg-check -- examples/iso_date_like.peg 2026-05-14
	! cabal exec peg-check -- examples/iso_date_like.peg 2026-13-14
	cabal exec peg-check -- examples/ipv4_like.peg 192.168.0.1
	! cabal exec peg-check -- examples/ipv4_like.peg 256.0.0.1
	cabal exec peg-check -- examples/email_like.peg user.name+tag@example.com
	! cabal exec peg-check -- examples/email_like.peg user@example
	cabal exec peg-check -- examples/url_like.peg https://example.com/a/b
	! cabal exec peg-check -- examples/url_like.peg ftp://example.com
	cabal exec peg-check -- examples/balanced_parentheses.peg "(()())"
	! cabal exec peg-check -- examples/balanced_parentheses.peg "(()"
	cabal exec peg-check -- --ast examples/anbncn.peg >/tmp/peg-monadic-parsers.ast
	cabal exec peg-check -- --emit-hs /tmp/GeneratedPEG.hs examples/anbncn.peg
	cabal exec ghc -- -fno-code /tmp/GeneratedPEG.hs

check: build test smoke

clean:
	rm -rf dist-newstyle
	rm -f GeneratedPEG.hs
