if exists("b:current_syntax")
    finish
endif

" Keywords
syntax keyword kw forall exists in of 
syntax keyword kw Given given Find find Satisfying satisfying

" Operators
syntax match op "&"
syntax match op "|"
syntax match op "\~"
syntax match op "->"
syntax match op "="
syntax match op "<="
syntax match op ">="
syntax match op ":"

" Comments
syntax match com "\v\(\*.*\*\)"

highlight link kw Keyword
highlight link op Operator
highlight link com Comment

let b:current_syntax = "prob"
