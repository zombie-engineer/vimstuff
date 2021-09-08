set list
set listchars=tab:>-
set tabstop=2
set shiftwidth=2
set expandtab
set number
set hlsearch
set cmdheight=3
syntax on
filetype plugin indent on

" Menu
set wildmenu
set wcm=<Tab>
menu ProjectDirSelection.ProjectLinuxKernel :let SessionProjectDir="/mnt/kernel/linux"<CR>
menu ProjectDirSelection.ProjectQEMU  :let SessionProjectDir="/mnt/ssd240/qemu-mainline"<CR>
map <F9> :emenu ProjectDirSelection.<Tab>


highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

if has("autocmd")
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

set sessionoptions=blank,buffers,curdir,folds,help,options,tabpages,winsize,globals

function! SkipNLines(n)
  call cursor(line('.') + a:n, 0)
endfunc

function! ToNextLine()
  call cursor(line('.') + 1, 0)
endfunc

function! ToPrevLine()
  call cursor(line('.') - 1, 0)
endfunc


function! ToLineN(n)
  call cursor(a:n, 0)
endfunc


function! ToLine0()
  call cursor(0, 0)
endfunc

function! FindFlag()
  let this_line = getline('.')
  let tokens = split(this_line, ' ')
  let flag_name = ""
  let new_tokens = []
  for el in tokens[1:]
    if el =~ "(EXT4_"
      let flag_name = split(split(el, '(')[0], ')')[0]
    else
      let new_tokens = new_tokens + [el]
    endif
  endfor
  let newline = "#define " . flag_name . " " . tokens[0] . " /*" . join(new_tokens) . " */"
  call setline(line('.'), newline)
endfunction

noremap <F5> :call FindFlag()<CR>
noh

noremap <F5> :source ~/.vimrc<CR>
function! SpacesToLines()
  let this_line = getline('.')
  let x = line('.')
  let tokens = split(this_line, ' ')
  for el in tokens
    call setline(x, el)
    let x = x + 1
  endfor
endfunction

noremap <F6> :call SpacesToLines()<CR>
"let SessionProjectDir = '/home/zombie/hackchi_round2/hakchi/r16-uboot/linux-9ed0e6c8612113834e9af9d16a3e90b573c488ca/'
"let SessionProjectDir = "/home/zombie/hackchi_round2/hakchi/r16-uboot-dir"
"let SessionProjectDir="/home/zombie/projects/raspberry/baremetal_aarch64"
let SessionProjectDir="/mnt/kernel/linux"


function! IsSymbolLine(line)
  return a:line =~ '[a-z0-9]* <[^ <>]*>:'
endfunction

function! MakeGlobl()
  let this_line = getline('.')
  let lineno = line('.')
  if IsSymbolLine(this_line) == 1
    let symbol_name = split(this_line, '<')[1]
    let symbol_name = split(symbol_name, '>')[0]
    let globlline = printf('.globl %s', symbol_name)
    echo lineno . ': ' . symbol_name . ' -> ' . globlline
    call setline(lineno, globlline)
    call append(lineno, symbol_name . ':')
  else
    echo lineno . ': ' . "line does not have symbol candidate"
  endif
endfunction
noremap <F7> :call MakeGlobl()<CR>


function! ValidateLineByRegex(l, rgx, desc, silent)
  if a:l =~ a:rgx
    return 1
  endif
  if ! a:silent
    echo printf('Not a "%s" line: "%s"', a:desc, a:l)
  endif
  return 0
endfunc

function! ValidatePcRelOp(l)
  return ValidateLineByRegex(a:l, '\(ldr\|str\).*[\t ]\+[A-Za-z][A-Za-z0-9],[\t ]\[pc.*\][\t ]\+;[\t ]*0x\x\+', 'pc_rel_op', 0)
endfunc

function! ValidateLabelLine(l)
  return ValidateLineByRegex(a:l, '^[a-zA-Z_0-9]\+:', "label", 0)
endfunc

function! ValidateBytecodeLine(l)
  return ValidateLineByRegex(a:l, '^ \{1,4}\x\+:', "bytecode", 0)
endfunc

function! ValidateBlLine(l)
  return ValidateLineByRegex(a:l, ' bl  [a-z0-9]* <[a-zA-Z0-9_]*>', 'bl_op', 0)
endfunc

function! ValidateLabelRefLine(l)
  return ValidateLineByRegex(a:l, ' b.* [a-z0-9]* <[a-zA-Z0-9_]*.0x[0-9a-z]*', 'label_ref', 0)
endfunc

function! ValidateLineBranching(l)
  return ValidateLineByRegex(a:l, '[ \t]b[a-z]*[\t ]0x[a-z0-9]\+', 'branching', 0)
endfunc

function! ValidateAsciiBytecodeLine(l, silent)
  " echo 'Validate ascii: ' . a:l
  if ! ValidateLineByRegex(a:l, '^ \{1,4}\x\+:', 'ascii_text', a:silent)
    if ! a:silent 
      echo 'Not a valid ascii line'
    endif
    return 0
  endif
  let s = split(a:l)[1]
  let s = printf("%08x", str2nr('0x' . s, 16))
  let c1 = s[0:1]
  let c2 = s[2:3]
  let c3 = s[4:5]
  let c4 = s[6:7]
  for c in [c1, c2, c3, c4]
    let n = str2nr('0x' . c, 16)
    let is_ascii = (n == 0 || (n >= 0x20 && n < 0x7f) || n == 0x0a || n == 0x0d || n == 0x09)
    if ! is_ascii
      echo 'Not a valid ascii line' . n . '0x' . c
      return 0
    endif
  endfor
  return 1
endfunc

function! ValidateLineSimple(line, allow_less_bytes)
  if a:allow_less_bytes
    return a:line =~ '^\s\+\x\{2,}:\s\x'
  endif
  return a:line =~ '^\s\+\x\{2,}:\s\x\{8}'
endfunc

function! GotoNextLabelRef()
  call search(' b.* [a-z0-9]* <[a-zA-Z0-9_]*.0x[0-9a-z]*')
endfunction


function! GetNextGlobl()
  let lineno = line('.')
  call search('.globl','W')
  let result = line('.')
  call cursor(lineno, 0)
  if result == lineno
    let result = line('$')
  endif
  return result
endfunction

function! GetPrevGlobl()
  let lineno = line('.')
  call search('.globl','bW')
  let result = line('.')
  call cursor(lineno, 0)
  if result == lineno
    result = 0
  endif
  return result
endfunction

function! EnsureLabelExists(addr, label_name)
  let this_line  = getline('.')
  let lineno     = line('.')
  let label_line = a:label_name . ':'
  let this_addr  = str2nr(split(this_line, ':')[0], 16)
  let label_addr = str2nr(a:addr, 16)
  if label_addr > this_addr
    call cursor(lineno + 1, 0)
    call search(a:addr . ':', 'W', GetNextGlobl())
  else
    call search(a:addr . ':', 'bW', GetPrevGlobl())
  endif


  if getline('.') == label_line
    echo printf("Label exists at line %d, skipping append", line('.') - 1)
  else
    let this_lineno = line('.')
    echo this_lineno
    call append(line('.') - 1, label_line)
    echo printf("Added new label at line: %d", line('.') - 1)
  endif
  call cursor(lineno, 0)
endfunction
  
function! MakeLabel()
  let this_line = getline('.')
  let lineno = line('.')
  if ! ValidateLabelRefLine(this_line)
    return
  endif
  let tokens = split(this_line, ' ')
  let label_address = tokens[-2]
  let label_subst = tokens[-2] . ' ' . tokens[-1]
  let label_name = split(tokens[-1], '+0x')[0][1:]
  let label_name = label_name . '_' . label_address
  let fixed_line = substitute(this_line, label_subst, label_name, 0)
  echo lineno . ': ' . label_address . ' : '. label_name . ' ->  ' . fixed_line
  call setline(lineno, fixed_line)
  call EnsureLabelExists(label_address, label_name)
endfunction

function! FixBl()
  let this_line = getline('.')
  let lineno = line('.')
  if ! ValidateBlLine(this_line)
    return
  endif
  let tokens = split(this_line, ' ')
  let label_address = tokens[-2]
  let label_name = tokens[-1][1:-2]
  let subst_from = 'bl.*$'
  let subst_to = printf("bl  %s", label_name)
  let fixed_line = substitute(this_line, subst_from, subst_to, '')
  echo lineno . ': ' . label_address . ' : '. label_name . ' ->  ' . fixed_line
  call setline(lineno, fixed_line)
endfunction

function! FixPcRelLabel()
  let this_line = getline('.')
  echo this_line
  let lineno = line('.')
  if ! ValidatePcRelOp(this_line)
    return
  endif
  let addr_description = split(split(split(this_line, ';')[1], '(')[1], ')')[0]
  let addr_string = split(addr_description, ' ')[0]
  let addr_name   = split(addr_description, ' ')[1][1:-2]
  let label_name = substitute(addr_name, '+0x.*', '_' . addr_string, '')
  let subst_from = ', [pc.*'
  let subst_to   = ', ' . label_name
  let fixed_line = substitute(this_line, subst_from, subst_to, '')
  echo printf('Substituting "%s" to "%s"', this_line, fixed_line)
  call setline(lineno, fixed_line)
  call EnsureLabelExists(addr_string, label_name)
endfunction

function! PcRelToLabel()
  let l = getline('.')
  if ! ValidateLabelLine(l)
    return
  endif
  
  let label = l[:-2]
  echo label
  let addrline = getline(line('.') + 1)
  if addrline =~ '^@ '
    let addrline = getline(line('.') + 2)
  endif
  if ! ValidateBytecodeLine(addrline)
    return
  endif
  echo addrline
  let addr = split(split(addrline, ':')[0])[0]
  let search_for = '; 0x' . addr
  echo search_for
  let saved_cursor = getcurpos()
  while 1
    let at_line = search(search_for, 'bW')
    if at_line == 0
      break
    endif 
    let this_line = getline('.')
    if ! ValidatePcRelOp(this_line)
      return
    endif
    call setline('.', substitute(this_line, '\[pc.*', label, ''))
    call setpos('.', saved_cursor)
    break
  endwhile
endfunc

function! RemoveByteCode()
  let lineno_end = line('$')
  let lineno_this = 1
  echo printf('Last line = %d', lineno_end)
  while lineno_this <= lineno_end
    let this_line = getline(lineno_this)
    if this_line =~ '[ \t]\+[a-z0-9]\{1,5\}:[ \t]\+'
      let fix_line = this_line[20:]
      call setline(lineno_this, fix_line)
      echo printf('line %d: "%s" -> "%s"', lineno_this, this_line, fix_line)
    endif
    let lineno_this = lineno_this + 1
  endwhile
endfunction

function! NumberToHex()
  let this_word = expand('<cword>')
  let hex_word = printf('0x%x', str2nr(this_word))
  let this_line = getline('.')
  let fixed_line = substitute(this_line, this_word, hex_word, 'g')
  call setline(line('.'), fixed_line)
endfunction

function! FixComment()
  let lineno_this = line('.')
  call cursor(lineno_this - 1, 0)
  let prev_line = search(' @ ', 'bW')
  if prev_line != 0
    let col_offset = getcurpos()[2] - 1
    call cursor(lineno_this, 0)
    let line_this = getline(lineno_this)
    echo printf("line: %s, length: %d, %d", line_this, len(line_this), col_offset)
    if len(line_this) < (col_offset + 1)
      let spaces = col_offset + 1 - len(line_this)
      let fixed_line = printf('%s%s@ ', line_this, repeat(' ', spaces))
      call setline(lineno_this, fixed_line)
      call cursor(lineno_this, len(fixed_line) + 1)
      call feedkeys('A', 'n')
    endif
  else
    echo "Unable to align"
  endif
endfunction

function! AlignComments(to_width)
  let lineno = line('.')
  while 1
    let this_line = getline(lineno)
    if this_line =~ '^.ascii'
      let tokens = split(this_line, '@')
      if len(tokens[0]) < a:to_width
        let padlen = a:to_width - len(tokens[0])
        let fixline = printf("%s%s@ %s", tokens[0], repeat(' ', padlen), tokens[1])
        call setline(lineno, fixline)
      endif
    else
      break
    endif
    let lineno = lineno + 1
  endwhile
endfunction

function! FindConstantDeclaration(addr)
  " echo "Finding " . a:addr
  let lineno = line('.')
  let x = search('.word ' . a:addr, 'nW')
  return split(getline(x - 1),':')[0]
endfunction

function! SubstitutePrintfConstant(addr, string, constIdx)
  let lineno = line('.')
  let constantLabel = FindConstantDeclaration(a:addr)
  let newConstantLabel = printf("str_const_%04d", a:constIdx)
  echo printf("Found label: %s, replacing all occurences to %s", constantLabel, newConstantLabel)
  call cursor(0, 0)
  while 1 
    let x = search(constantLabel, 'We')
    if x == 0 
      break
    endif

    let this_line = getline('.')
    echo printf("Found match at line: %d (%s)", x, this_line)
    let fixline = substitute(this_line, constantLabel, newConstantLabel, 'g')
    if fixline =~ 'ldr '
      let fixline = printf('%s @ %s', fixline, a:string)
    endif
    echo printf("  replacing '%s' to '%s'", this_line, fixline)
    call setline('.', fixline)
  endwhile
  call cursor(lineno, 0)
endfunction

function! NamePrintfConstants()
  let lineno = line('.')
  let constIdx = 1
  while 1
    let this_line = getline(lineno)
    if this_line =~ '^.ascii'
      let addr = split(this_line, '@')[1][1:]
      let string = split(this_line, '\\0')[0][7:] . '"'
      call SubstitutePrintfConstant(addr, string, constIdx)
    else
      break
    endif
    let lineno = lineno + 1
    call cursor(lineno, 0)
    let constIdx = constIdx + 1
  endwhile
endfunction


let locations = {}
let locations['board_init_f'] = ['arch/arm/lib/board.c', 'void board_init_f(ulong bootflag)']
let locations['global_data'] = ['include/asm/global_data.h', 'struct  global_data {']
let locations['bg_t'] = ['include/asm/u-boot.h', 'struct']

function! Jump(location)
  if has_key(g:locations, a:location)
    let loc = g:locations[a:location]
    let loc_file = loc[0]
    let loc_search_patt = loc[1]
    echo loc_file
    execute(printf("tabe %s", loc_file))
    let rc = search(loc_search_patt) 
    if rc == 0
      echo 'Failed to find location in file'
    endif
  else
    echo 'Unknown location'
  endif    
endfunc

function! ListLocations()
  for i in keys(g:locations)
    echo i . ": " . g:locations[i][0] . ", " . g:locations[i][1]
  endfor
endfunc

function! GrepGetIncludeString(only_headers)
  if a:only_headers
    return '--include=*.h'
  endif
  return '--include=*.[hcS]'
endfunc

function! Exec(cmd)
  redir =>output
  silent exec a:cmd
  redir END
  return output
endfunc

function! CreateScratchBuf()
  :new
  :setlocal buftype=nofile
  :setlocal bufhidden=wipe
  :setlocal noswapfile
endfunc

function! OpenGrepResult()
  let l = getline('.')
  let filepath = split(l, ':')[0]
  let lineno = split(l, ':')[1]
  silent exe "tabe " . filepath
  call cursor(lineno, 0)

  echo filepath . "--" . lineno
endfunction

function! GrepInProject(token, only_headers, additional_filter)
  let cmd = '!grep -R -n'
  let cmd = cmd . ' ' . GrepGetIncludeString(a:only_headers)
  let cmd = cmd . ' ' . a:token
  let cmd = cmd . ' ' . g:SessionProjectDir
  if len(a:additional_filter) > 0
    let cmd = cmd . ' | grep ' . a:additional_filter
  endif
  let cmd = cmd . ' | less'
  echo cmd
  "let cmd="!grep -Rn --include=*.[ch] try_charge /mnt/kernel/linux/"
  call CreateScratchBuf()
  silent exe "read " . cmd
  :1delete
  return
endfunc

function! HexToAscii(hexstring)
  let byte_count = len(a:hexstring) / 2
  let byte_pos = 0
  let result = []
  while byte_pos < byte_count
    let c1 = str2nr(a:hexstring[byte_pos * 2 : byte_pos * 2 + 1], 16)
    call add(result, printf("%c", c1))
    let byte_pos = byte_pos + 1
  endwhile
  echo join(result, '')
endfunc

function! ShowSrc()
  let srcline = split(split(getline('.'))[1], ':')
  let src = srcline[0]
  let ln = srcline[1]
  wincmd w
  let path = '/home/zombie/hackchi_round2/hakchi/r16-uboot-dir/'
  let path = '/home/zombie/hackchi_round2/hakchi/r16-uboot/linux-9ed0e6c8612113834e9af9d16a3e90b573c488ca/'
  let path = '/home/zombie/SuperNintendoMiniOSS/r16-uboot-fc3061df4dbd4153819b2d2f141d82b88fea51cf/'

  exe 'edit ' . path . src
  exe 'norm ' . ln . 'G'
  exe 'norm zt'
  wincmd w
endfunc

function! IsUppercase(ch)
  return a:ch >= 'A' && a:ch <= 'Z'
endfunc

function! CamelcaseToUnderscore()
  let w = expand('<cword>')
  if len(w) == 0
    return
  endif

  let last_case = IsUppercase(w[0])
  let w2 = ''
  
  for ch in split(w, '\zs')
    if last_case == 0 && IsUppercase(ch) == 1
      let w2 = w2 . '_'
    endif
    let last_case = IsUppercase(ch)
    let w2 = w2 . ch
  endfor
  let res = tolower(w2)
  let @r = res
  norm viw"rp
endfunc

function! CountStructBits()
  let marked = getcurpos()
  call search('{', 'b')
  let l1 = line('.')
  norm %
  let l2 = line('.')
  let l = l1 + 1
  let bits = 0
  while l < l2
    let bit = split(split(getline(l), ':')[1], ';')[0]
    let bit = str2nr(bit, 10)
    let bits = bits + bit
    let l = l + 1
  endwhile
  call setpos('.', marked)
  echo bits
endfunc

function! BytecodeLineGetAddress(l)
  if ! ValidateLineSimple(a:l, 1)
    echo 'Not a bytecode line'
    return ''
  endif
  return split(split(a:l)[0],':')[0]
endfunc

function! BytecodeLineGetBytecode(l)
  if ! ValidateLineSimple(a:l, 1)
    echo 'Not a bytecode line'
    sleep 1
    return 0
  endif
  let bytecode_str = split(a:l)[1]
  return str2nr('0x' . bytecode_str, 16)
endfunc

function! BytecodeLineToTokens()
  let l = getline('.')
  if ! ValidateLineSimple(l, 1)
    return 0
  endif
  let result = []
  let addr = str2nr(BytecodeLineGetAddress(l), 16)
  let bytecode = BytecodeLineGetBytecode(l)
  return [addr, bytecode]
endfunc

function! BytecodeLineGetLabel(l)
  return a:l[:-2]
endfunc

function! BytecodeLineIsBytecode(l)
  return ValidateLineSimple(a:l, 1)
endfunc

function! AsciiStringToLabel(ascii_str)
  let str = 'text_'
  let str_len = len(a:ascii_str)
  let pos = 0
  while pos < str_len
    let ch = a:ascii_str[pos]
    if (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
      let ch = ch
    else
      let ch = '_'
    endif
    let str = str . ch
    let pos = pos + 1
  endwhile
  return str
endfunc

function! BytecodeDeclarePointerLabel()
  let l = line('.')
  let bytecode_tokens = BytecodeLineToTokens()
  let addr = printf("%x:", bytecode_tokens[1])
  call ToLine0()
  if !search(addr)
    echo 'Address not found'
    return
  endif
  let label = ''
  while 1
    call ToPrevLine()
    let l = getline('.')
    if ValidateLabelLine(getline('.'))
      let label = l[:-1]
      break
    endif
  endwhile
  call append(l - 1, printf('p_%s', label))
  call ToLineN(l + 2)
endfun

function! BytecodeLineMakeSimple(padding, addr, bytes)
  let bt = a:bytes
  if len(bt) % 2
    let bt = '0' . bt
  endif
  return printf("%s%x: %s", repeat(' ', a:padding), a:addr, bt)
endfunc

function! BytecodeLineGetPadding(l)
  let i = 0
  while a:l[i] == ' '
    let i += 1
  endwhile
  return i
endfunc

function! BytecodeLineSplitLine(pos)
  if a:pos < 1 || a:pos > 3
    echo printf('Impossible to split bytecode line with this index: %d', a:pos)
    return 0
  endif
  let l = getline('.')
  let padding = BytecodeLineGetPadding(l)
  let addr = str2nr(BytecodeLineGetAddress(l), 16)
  let this_bytecode = printf("%08x", BytecodeLineGetBytecode(l))
  " echo l . ' -> ' . this_bytecode
  let split_bytecode_1 = this_bytecode[a:pos * 2:]
  let split_bytecode_2 = this_bytecode[:a:pos * 2 - 1]
  let addr2 = addr + 4 - a:pos
  " echo printf('SplitLine: %x: %s -> %x: %s; %x: %s', addr, this_bytecode, addr, split_bytecode_1, addr2, split_bytecode_2)
  call setline('.', BytecodeLineMakeSimple(padding, addr, split_bytecode_1))
  call append('.', BytecodeLineMakeSimple(padding, addr2, split_bytecode_2))
  return 1
endfunc

function! ToNextAsciiBytecode()
  let i = 0
  while i < 200
    call ToNextLine()
    let l = getline('.')
    if ValidateAsciiBytecodeLine(l, 1)
      let x = BytecodeLineGetBytecode(l)
      if x 
        break
      endif
    endif
  endwhile
endfunc

function! BytecodeLineBytecodeToText(l)
  let s0 = split(a:l)[1]
  let s = printf("%08x", str2nr('0x' . s0, 16))
  "echo s
  let c1 = s[0:1]
  let c2 = s[2:3]
  let c3 = s[4:5]
  let c4 = s[6:7]
  let zero_idx = -1
  let i = 0
  let str = ''
  let missing =  4 - (len(s0) + 1) / 2
  "echo missing
  let strings = [c4, c3, c2, c1][: - (1 + missing)]
  "echo strings
  for c in strings
    let n = str2nr('0x' . c, 16)
    if ! n
      let zero_idx = 3 - i
      break
    endif
    let is_ascii = (n >= 0x20 && n < 0x7f) || n == 0xa || n == 0xd || n == 0x9
    if ! is_ascii
      echo 'Error - not an ascii symbol: ' . n . ', 0x' . c
      return ['BADBAD', -1]
    endif
    let i += 1
    let str = str . printf('%c', n)
  endfor
  return [str, zero_idx]
endfunc

function! BytecodeProcessSingleString()
  let fulltext = ''
  while 1
    let l = getline('.')
    if ! ValidateAsciiBytecodeLine(l, 0)
      break
    endif

    let addr = str2nr(BytecodeLineGetAddress(l), 16)
    let bytecode = printf("%x", BytecodeLineGetBytecode(l))
    let text = BytecodeLineBytecodeToText(l)
    " echo text
    let zero_idx = text[1]
    let text = text[0]
    let fulltext = fulltext . text
    let padding = BytecodeLineGetPadding(l)
    " echo 'text: ' . text . ', z: ' . zero_idx
    if zero_idx != -1
      " echo printf('Zero term: addr: %x, text_length: %d, split pos: %d', addr, len(text) , zero_idx)
      if zero_idx > 0
        call BytecodeLineSplitLine(zero_idx)
      else
        call setline('.', BytecodeLineMakeSimple(padding, addr, bytecode))
      endif
      break
    else
      call setline('.', BytecodeLineMakeSimple(padding, addr, bytecode))
    endif
    call ToNextLine()
  endwhile
  call append(line('.'), '')
  call ToNextLine()
  return fulltext
endfunc

function! BytecodeDowngradeToSimple()
  let l = getline('.')
  let addr = str2nr(BytecodeLineGetAddress(l), 16)
  let bytecode = printf("%08x", BytecodeLineGetBytecode(l))
  let padding = BytecodeLineGetPadding(l)
  call setline('.', BytecodeLineMakeSimple(padding, addr, bytecode))
endfunc

function! BytecodeProcessStrings(limit)
  let i = 0
  while i < a:limit
    let lineno = line('.')
    let text = BytecodeProcessSingleString()
    if len(text)
      let label = AsciiStringToLabel(text)
      call append(lineno - 1, label . ':')
      call append(lineno, '@ ' . text)
      let i = i + 1
      call ToNextLine()
    else
      break
    endif
  endwhile
endfunc

function! TryFixTextLabel()
  let l = getline('.')
  let bytecode = printf("%08x", BytecodeLineGetBytecode(l))
  if bytecode =~ '^47'
    let search_for= printf("%x:", str2nr(bytecode[2:], 16))
    " echo 'Searching for ' . search_for 
    let saved_pos = getcurpos()
    if search(search_for, 'ws')
      " echo "Found"
      call ToPrevLine()
      let this_comment = getline('.')
      if this_comment =~ '^@ '
        call ToPrevLine()
        let this_l = getline('.')
        if this_l =~ '^text_'
          " echo "Text"
          call setpos('.', saved_pos)
          call ToPrevLine()
          call append('.', this_comment)
          call append('.', 'p_' . this_l)
          call SkipNLines(4)
          return
        endif
      endif
    endif
    call setpos('.', saved_pos)
  endif
endfunc

function! RenameBlAddrToFuncName()
  let saved_pos = getcurpos()
  let l = getline('.')
  let funcname = ''
  if ValidateLabelLine(l)
    let funcname = BytecodeLineGetLabel(l)
    while ! BytecodeLineIsBytecode(l)
      call ToNextLine()
      let l = getline('.')
    endwhile
    let addr = BytecodeLineGetAddress(l)
  else
    echo 'Put cursor on a function name label first'
    return
  endif
  echo printf("addr: %s, funcname: %s", addr, funcname)
  let for_subs = '0x' . addr
  call setpos('.', [0,0,0,0])
  while search(for_subs . '\>', 'sW')
    let cl = getline('.')
    echo cl
    call setline('.', substitute(cl, for_subs, funcname, ''))
  endwhile
  call setpos('.', saved_pos)
endfunc

function! PutConditinalBranchLabel()
  let saved_pos = getcurpos()
  let l = getline('.')
  if ! ValidateLineBranching(l)
    return ''
  endif
  let branch_addr = split(l)[3]
  let search_for = '\<' . branch_addr[2:] . ':'
  echo branch_addr
  echo search_for
  if search(search_for, 'sw')
    call append(line('.') - 1, branch_addr[2:] . ':')
  endif
  call setpos('.', saved_pos)
endfunc  

function! LoadPageMapAtOffset(o)
  let cmd = printf('!hexdump -C -s %d -n 2048 nand_dump.bin| less', a:o)
  execute(cmd)
endfunc

function! AlignSelected()
  let width = 36
  let lstart = line("'<")
  let lend   = line("'>")
  while lstart <= lend
    let l = split(getline(lstart))
    " echo lstart . ":" . l[2]
    let newl = printf("%s %-*s 0x%02x", l[0], width, l[1], str2nr(l[2], 16))
    call setline(lstart, newl)
    let lstart += 1
  endwhile
endfunc

function! AlignColumns()
  while search("^USB_HPR", 'W') > 0
    let l = split(getline('.'))
    let bitname = l[0]
    let bitpos  = str2nr(l[1])
    let comments = ' '.join(l[2:])
    let newl = printf("#define %-36s %2d// %s", bitname, bitpos, comments)
    echo newl
    call setline('.', newl)
  endwhile
endfunc



let mapleader = "t"

" Search throughout the whole project
noremap <Leader>ga :call GrepInProject("<C-R><C-W>", 0, '') <CR>
" Search only related to clover board
noremap <Leader>4 :call GrepInProject("<C-R><C-W>", 0, 'clover')<CR>
" Search only headers
noremap <Leader>gh :call GrepInProject("<C-R><C-W>", 1, '') <CR>

noremap <Leader>gd :execute printf("!git diff %s", expand('%p'))<CR>

noremap <F8> :call MakeLabel()<CR>
noremap <Leader>1 :call Jump('board_init_f')<CR>
noremap <Leader>2 :call Jump('global_data')<CR>
noremap <Leader>3 :call Jump('bg_t')<CR>
noremap <Leader>b :call FixBl()<CR>
noremap <Leader>v :call PcRelToLabel()<CR>
noremap <Leader>c :call RemoveByteCode()<CR>
noremap <Leader>n :call NumberToHex()<CR>
noremap <Leader>t :call FixComment()<CR>
noremap <Leader>q :call ShowSrc()<CR>
noremap <Leader>ll :call CamelcaseToUnderscore()<CR>
noremap <Leader>5 :!bitdescriptor <C-R><C-W><CR>
noremap <F7> :!make<CR>
" noremap <Leader>q :call AlignComments(70)<CR>
noremap <Leader>1 :call NamePrintfConstants()<CR>
"noremap <F9> :call GotoNextLabelRef()<CR>
noremap hj :call BytecodeDeclarePointerLabel()<CR>
noremap ha :call HexToAscii("<C-R><C-W>")<CR>
"noremap <F2> :call CountStructBits()<CR>
"noremap <F2> :call PutConditinalBranchLabel()<CR>
"noremap <F2> :call LoadPageMapAtOffset(0x<C-R><C-W> / 64 * 2048)<CR>
"noremap <F2> :!./update.sh<CR>
"noremap <F2> :w<CR>:!python3 c1.py<CR>
"noremap <F2> :w<CR>:make g<CR>
noremap <F2> :w<CR>:make T=1 upd<CR>
noremap <F3> :w<CR>:make T=2 upd<CR>
noremap <F4> :w<CR>:make T=3 upd<CR>
noremap qw :call OpenGrepResult()<CR>
"noremap <F2> :w<CR>:!make && make jr && make ju && make jd<CR>
"noremap <F3> :call BytecodeProcessStrings(10)<CR>:call ToNextAsciiBytecode()<CR>
"noremap <F3> :call TryFixTextLabel()<CR>
"noremap <F3> :call system('./parse_uart_capture.py uart_capture.bin \| less')<CR>
"noremap <F3> :!./parse_uart_capture.py uart_capture.bin \| less<CR>
"noremap <F3> :call AlignColumns()<CR>
"noremap <F3> :!aarch64_objdump -dS kernel8.elf \| less<CR>
"noremap <F3> :w<CR>:!make jr && make jd<CR>
"noremap <F4> :call RenameBlAddrToFuncName()<CR>

noremap <F5> :call GrepInProject("<C-R><C-W>", 0, "")<CR>
" ==========================================
" SYNTAX
"
au Syntax *.disas_arm runtime! syntax/arm_asm.vim
