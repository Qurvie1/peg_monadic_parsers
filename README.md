# peg_monadic_parsers

Проект реализует проверку принадлежности строки языку, заданному PEG-грамматикой. 

Описание грамматики парсится в AST, валидируется, затем синтаксически транслируется в монадический PEG-парсер. 

## Как были подготовлены examples

Файлы в `examples/` не генерируются автоматически. Это набор вручную написанных PEG-грамматик для демонстрации разных возможностей парсера.

Классические примеры (`anbn.peg`, `anbncn.peg`, `not_anbn.peg`, `balanced_parentheses.peg`) показывают рекурсию, lookahead, отрицание и языки за пределами регулярных. Остальные примеры (`identifier.peg`, `number.peg`, `hex_color.peg`, `iso_date_like.peg`, `ipv4_like.peg`, `email_like.peg`, `url_like.peg`, `simple_expression.peg`) были составлены как компактные recognizer-грамматики для распространённых форматов.

Каждый example проверяется отдельно через CLI, например:

```bash
cabal exec peg-check -- examples/ipv4_like.peg 192.168.0.1
cabal exec peg-check -- examples/email_like.peg user.name+tag@example.com