" $Id: rfc5424.vim,v 0.1.0 2011/08/02 Kyan Exp $
" Vim syntax file
" Language:     RFC5424 syslog log file
" Maintainer:   Kyan He <kyan.ql.he@gmail.com>
" Last Change:  $Date: 2011/08/12 14:35:00 $
" Remark:

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match   syslogMsg ".*$"
syn match   syslogSd "[^ ]\+" nextgroup=syslogMsg skipwhite
syn match   syslogMsgid "[^ ]\+" nextgroup=syslogSd skipwhite
syn match   syslogProcid "[^ ]\+" nextgroup=syslogMsgid skipwhite
syn match   syslogAppname "[^ ]\+" nextgroup=syslogProcid skipwhite
syn match   syslogHostname "[^ ]\+" nextgroup=syslogAppname skipwhite
syn match   syslogTimestamp "\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d\(\.\d\{1,3\}Z\)\?" skipwhite nextgroup=syslogHostname
syn match   syslogVersion "\d " nextgroup=syslogTimestamp
syn match   syslogPriority "^<\d\d>" nextgroup=syslogVersion

if !exists("did_syslog_syntax_inits")
  let did_syslog_syntax_inits = 1
  hi link syslogPriority    Statement
  hi link syslogVersion     String
  hi link syslogTimestamp   Comment
  hi link syslogHostname    Identifier
  hi link syslogAppname     Comment
  hi link syslogProcid      Statement
  hi link syslogMsgid       Type
  hi link syslogSd          Comment
  hi link syslogMsg         String
endif

let b:current_syntax="syslog"

