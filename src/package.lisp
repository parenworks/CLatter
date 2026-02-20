(in-package #:cl-user)

;;; ============================================================
;;; CLatter Package Definitions - ANSI Version
;;; ============================================================

(defpackage #:clatter
  (:use #:cl)
  (:export #:main))

;;; ============================================================
;;; ANSI Terminal Package
;;; ============================================================

(defpackage #:clatter.ansi
  (:use #:cl)
  (:export
   #:*escape*
   #:color #:indexed-color #:rgb-color #:named-color
   #:make-indexed-color #:make-rgb-color #:make-named-color
   #:color-index #:color-red #:color-green #:color-blue #:color-name
   #:*color-palette* #:register-color #:lookup-color
   #:emit-fg #:emit-bg
   #:text-style #:make-style
   #:style-fg #:style-bg #:style-bold-p #:style-dim-p
   #:style-italic-p #:style-underline-p #:style-inverse-p
   #:emit-style
   #:cursor-to #:cursor-home #:cursor-hide #:cursor-show
   #:cursor-up #:cursor-down #:cursor-forward #:cursor-back
   #:clear-screen #:clear-line #:clear-to-eol #:reset
   #:begin-sync-update #:end-sync-update
   #:begin-hyperlink #:end-hyperlink #:hyperlink
   #:enter-alternate-screen #:leave-alternate-screen
   #:fg #:bg #:fg-rgb #:bg-rgb
   #:bold #:dim #:italic #:underline #:inverse
   #:write-at #:write-styled #:with-style
   #:draw-box #:fill-rect))

;;; ============================================================
;;; Terminal Input Package
;;; ============================================================

(defpackage #:clatter.terminal
  (:use #:cl)
  (:export
   #:key-event #:make-key-event
   #:key-event-char #:key-event-code #:key-event-ctrl-p #:key-event-alt-p
   #:key-event-mouse-x #:key-event-mouse-y
   #:+key-up+ #:+key-down+ #:+key-left+ #:+key-right+
   #:+key-enter+ #:+key-escape+ #:+key-tab+ #:+key-backspace+
   #:+key-delete+ #:+key-home+ #:+key-end+
   #:+key-page-up+ #:+key-page-down+ #:+key-mouse+ #:+key-resize+
   #:terminal-mode #:*terminal-mode*
   #:enable-raw-mode #:disable-raw-mode #:query-size
   #:terminal-raw-p #:terminal-width #:terminal-height
   #:terminal-size #:with-raw-terminal
   #:enable-mouse-tracking #:disable-mouse-tracking
   #:input-reader #:*input-reader*
   #:reader-open #:reader-close #:read-key-event
   #:read-key #:read-key-with-timeout
   #:close-tty-stream))

;;; ============================================================
;;; Core Packages (unchanged from original)
;;; ============================================================

(defpackage #:clatter.core.constants
  (:use #:cl)
  (:export
   #:+default-scrollback-capacity+ #:+default-buflist-width+ #:+default-nicklist-width+
   #:+max-recent-urls+
   #:+health-check-interval+ #:+typing-throttle-seconds+
   #:+ping-timeout-seconds+ #:+pong-timeout-seconds+
   #:+reconnect-min-delay+ #:+reconnect-max-delay+
   #:+irc-max-line-length+ #:+irc-safe-message-length+ #:+irc-max-channel-length+
   #:+dcc-port-range-start+ #:+dcc-port-range-end+ #:+dcc-timeout-seconds+ #:+dcc-buffer-size+
   #:+rpl-welcome+ #:+rpl-yourhost+ #:+rpl-created+ #:+rpl-myinfo+ #:+rpl-isupport+
   #:+rpl-namreply+ #:+rpl-endofnames+ #:+rpl-topic+ #:+rpl-topicwhotime+
   #:+rpl-motd+ #:+rpl-motdstart+ #:+rpl-endofmotd+
   #:+rpl-whoisuser+ #:+rpl-whoisserver+ #:+rpl-whoisoperator+ #:+rpl-whoisidle+
   #:+rpl-endofwhois+ #:+rpl-whoischannels+ #:+rpl-whoisaccount+
   #:+err-nicknameinuse+
   #:+crafterbin-url+
   #:+wanted-capabilities+))

(defpackage #:clatter.core.debug
  (:use #:cl)
  (:export
   #:+level-off+ #:+level-error+ #:+level-warn+ #:+level-info+ #:+level-debug+ #:+level-trace+
   #:*debug-level* #:level-name
   #:set-debug-level #:debug-status
   #:enable-debug-category #:disable-debug-category #:debug-category-enabled-p 
   #:list-debug-categories #:clear-debug-categories
   #:open-debug-file #:close-debug-file
   #:debug-log #:log-error #:log-warn #:log-info #:log-debug #:log-trace
   #:log-irc-raw #:log-irc-event))

(defpackage #:clatter.core.ring
  (:use #:cl)
  (:export #:make-ring #:ring-push #:ring->list #:ring-count))

(defpackage #:clatter.core.model
  (:use #:cl)
  (:import-from #:clatter.core.ring #:make-ring #:ring-push #:ring->list #:ring-count)
  (:export
   #:app #:make-app #:app-ui #:app-buffers #:app-current-buffer-id #:app-buffer-order #:app-connections #:app-dirty-flags #:app-quit-requested
   #:mark-dirty #:dirty-p #:clear-dirty
   #:buffer #:server-buffer #:channel-buffer #:query-buffer #:dcc-buffer
   #:make-buffer #:buffer-id #:buffer-title #:buffer-kind #:buffer-network #:buffer-scrollback
   #:buffer-dcc-connection
   #:create-server-buffer
   #:buffer-unread-count #:buffer-highlight-count #:buffer-scroll-offset #:buffer-members #:buffer-typing-users #:get-typing-nicks
   #:buffer-filter-pattern #:buffer-filter-active #:buffer-recent-urls
   #:extract-urls #:buffer-add-urls #:open-url #:+max-recent-urls+
   #:buffer-channel-modes #:buffer-my-modes
   #:ui-win-chat2 #:ui-win-nicklist #:ui-split-mode #:ui-split-buffer-id #:ui-active-pane
   #:ui-nicklist-w #:ui-nicklist-visible
   #:message #:make-message #:message-ts #:message-level #:message-nick #:message-text #:message-highlight
   #:ui-state #:make-ui-state #:ui-input #:ui-screen
   #:ui-win-buflist #:ui-win-chat #:ui-win-status #:ui-win-input
   #:ui-term-w #:ui-term-h #:ui-buflist-w
   #:input-state #:make-input-state #:input-text #:input-cursor #:input-history #:input-history-pos
   #:find-buffer #:current-buffer #:active-buffer
   #:get-buffer-connection #:get-current-connection
   #:remove-buffer #:compact-buffers #:find-buffer-by-title
   #:app-ignore-list #:ignore-nick #:unignore-nick #:ignored-p #:list-ignored
   #:buffer-add-member #:buffer-remove-member #:buffer-has-member-p #:buffer-member-list
   #:app-buffers-list #:find-buffer-by-network #:create-buffer))

(defpackage #:clatter.core.config
  (:use #:cl)
  (:export
   #:*config-dir* #:*config-file*
   #:network-config #:make-network-config
   #:network-config-name #:network-config-server #:network-config-port
   #:network-config-tls #:network-config-nick #:network-config-username
   #:network-config-realname #:network-config-password #:network-config-nickserv-pw
   #:network-config-sasl #:network-config-client-cert #:network-config-autojoin #:network-config-autoconnect
   #:config #:make-config #:config-networks #:config-default-network #:config-time-format #:config-buflist-width
   #:load-config #:save-config #:find-network-config #:add-network-config
   #:default-libera-config #:get-network-password #:get-server-password #:lookup-authinfo))

(defpackage #:clatter.core.protocol
  (:use #:cl)
  (:export
   #:irc-message #:make-irc-message
   #:irc-message-tags #:irc-message-prefix #:irc-message-command #:irc-message-params
   #:parse-irc-line #:format-irc-line
   #:parse-irc-tags #:get-server-time #:parse-iso8601-time
   #:parse-prefix #:prefix-nick #:strip-irc-formatting
   #:sanitize-irc-input #:validate-irc-input
   #:channel-prefix-p #:channel-name-p #:valid-channel-name-p
   #:+irc-max-line-length+ #:+irc-safe-message-length+
   #:message-overhead #:max-message-length #:split-long-message
   #:irc-nick #:irc-user #:irc-pass #:irc-join #:irc-part
   #:irc-privmsg #:irc-notice #:irc-quit #:irc-pong #:irc-ping #:irc-cap
   #:irc-whois #:irc-topic #:irc-kick #:irc-mode #:irc-away #:irc-ctcp-reply
   #:irc-tagmsg #:irc-typing #:irc-invite #:irc-names
   #:irc-monitor-add #:irc-monitor-remove #:irc-monitor-clear #:irc-monitor-list #:irc-monitor-status
   #:+rpl-welcome+ #:+rpl-yourhost+ #:+rpl-created+ #:+rpl-myinfo+ #:+rpl-isupport+
   #:+rpl-namreply+ #:+rpl-endofnames+ #:+rpl-motd+ #:+rpl-motdstart+ #:+rpl-endofmotd+
   #:+rpl-topic+ #:+rpl-topicwhotime+ #:+err-nicknameinuse+
   #:+rpl-whoisuser+ #:+rpl-whoisserver+ #:+rpl-whoisoperator+
   #:+rpl-whoisidle+ #:+rpl-endofwhois+ #:+rpl-whoischannels+ #:+rpl-whoisaccount+))

(defpackage #:clatter.core.commands
  (:use #:cl)
  (:export
   #:*current-connection* #:*current-config*
   #:parse-command #:execute-command #:handle-input-line #:show-help))

(defpackage #:clatter.core.events
  (:use #:cl)
  (:export
   #:irc-event #:handle-event
   #:event-connection #:event-timestamp #:event-raw-message
   #:connect-event #:event-server #:event-nick
   #:disconnect-event #:event-reason
   #:message-event #:event-sender #:event-target #:event-text #:event-server-time
   #:privmsg-event #:notice-event #:action-event
   #:channel-event #:event-channel
   #:join-event #:event-account #:event-realname
   #:part-event #:event-message
   #:quit-event
   #:kick-event #:event-kicked-nick
   #:nick-event #:event-old-nick #:event-new-nick
   #:topic-event #:event-topic
   #:mode-event #:event-setter #:event-modes
   #:away-event
   #:ctcp-event #:event-command #:event-args
   #:dcc-offer-event #:event-dcc-type #:event-filename #:event-ip #:event-port #:event-filesize
   #:names-event #:event-names
   #:numeric-event #:event-numeric #:event-params
   #:typing-event #:event-state
   #:ev #:ev-type #:ev-plist))

(defpackage #:clatter.core.logging
  (:use #:cl)
  (:export
   #:*log-base-dir* #:*logging-enabled* #:*current-network*
   #:log-message #:write-log-entry
   #:read-recent-logs #:search-logs #:list-logged-targets #:list-log-files
   #:export-logs #:export-logs-text #:export-logs-json #:export-logs-html))

(defpackage #:clatter.core.dispatch
  (:use #:cl)
  (:import-from #:clatter.core.model
                #:app #:app-ui #:app-current-buffer-id #:buffer #:buffer-id #:message
                #:mark-dirty #:current-buffer #:find-buffer
                #:buffer-scrollback #:buffer-unread-count #:buffer-highlight-count
                #:buffer-scroll-offset #:buffer-add-urls
                #:ui-split-mode #:ui-split-buffer-id)
  (:import-from #:clatter.core.ring #:ring-push)
  (:export #:apply-event #:deliver-message))

;;; ============================================================
;;; UI Packages - ANSI Version
;;; ============================================================

(defpackage #:clatter.ui.theme
  (:use #:cl)
  (:export
   #:base-theme #:tokyo-night-theme #:dark-theme #:light-theme #:ascii-theme #:rounded-theme
   #:theme-nick-colors #:theme-bg #:theme-fg
   #:theme-border-active #:theme-border-inactive
   #:theme-unread-indicator #:theme-mention-indicator #:theme-timestamp
   #:theme-join-color #:theme-part-color #:theme-error-color #:theme-system-color
   #:theme-presence-color #:theme-action-color
   #:theme-input-prompt
   #:theme-box-h #:theme-box-v #:theme-box-tl #:theme-box-tr #:theme-box-bl #:theme-box-br
   #:theme-box-t-down #:theme-box-t-up #:theme-box-t-right #:theme-box-t-left #:theme-box-cross
   #:theme-connected-indicator #:theme-connecting-indicator #:theme-disconnected-indicator
   #:theme-tls-indicator #:theme-scroll-indicator #:theme-filter-indicator
   #:theme-level-color #:theme-nick-color
   #:*theme-registry* #:*current-theme*
   #:register-theme #:find-theme #:list-themes #:current-theme #:set-theme))

(defpackage #:clatter.ui.widgets
  (:use #:cl)
  (:export
   #:panel #:panel-x #:panel-y #:panel-width #:panel-height
   #:panel-visible-p #:panel-border-p #:panel-title #:panel-active-p
   #:panel-render #:panel-clear
   #:panel-content-x #:panel-content-y #:panel-content-width #:panel-content-height
   #:buflist-panel #:buflist-app
   #:chat-panel #:chat-buffer #:chat-time-format
   #:nicklist-panel #:nicklist-buffer
   #:status-panel #:status-app #:status-buffer
   #:input-panel #:input-panel-state))

(defpackage #:clatter.ui.layout
  (:use #:cl)
  (:export
   #:layout #:make-layout
   #:layout-buflist-width #:layout-nicklist-width #:layout-nicklist-visible
   #:layout-split-mode #:layout-term-width #:layout-term-height
   #:layout-buflist #:layout-chat-a #:layout-chat-b #:layout-nicklist
   #:layout-status #:layout-input
   #:layout-compute #:layout-render #:layout-update-buffers))

(defpackage #:clatter.ui.input
  (:use #:cl)
  (:import-from #:clatter.core.model
                #:app #:app-ui #:current-buffer #:mark-dirty
                #:input-state #:input-text #:input-cursor #:input-history #:input-history-pos
                #:ui-input)
  (:export
   #:input-insert-char #:input-backspace #:input-delete
   #:input-move-left #:input-move-right #:input-move-home #:input-move-end
   #:input-history-prev #:input-history-next
   #:input-submit-line #:input-set-text #:input-tab-complete))

(defpackage #:clatter.ui.render
  (:use #:cl)
  (:export #:render-frame))

(defpackage #:clatter.ui.tui
  (:use #:cl)
  (:export 
   #:run-tui 
   #:ui-submit
   #:*layout* #:*running*))

;;; ============================================================
;;; Network Packages
;;; ============================================================

(defpackage #:clatter.net.irc
  (:use #:cl)
  (:export
   #:irc-connection #:make-irc-connection
   #:irc-connect #:irc-disconnect #:irc-send
   #:irc-state #:irc-nick #:irc-network-config
   #:irc-reconnect-enabled
   #:irc-check-health
   #:irc-cap-enabled
   #:irc-send-typing
   #:irc-request-chathistory
   #:irc-send-labeled
   #:irc-app #:irc-network-id
   #:start-irc-connection))

(defpackage #:clatter.net.dcc
  (:use #:cl)
  (:export
   #:dcc-manager #:make-dcc-manager #:*dcc-manager*
   #:dcc-manager-add #:dcc-manager-remove #:dcc-manager-find
   #:dcc-manager-list #:dcc-manager-pending #:dcc-manager-connections
   #:dcc-connection #:dcc-chat #:dcc-send
   #:dcc-id #:dcc-nick #:dcc-state #:dcc-direction #:dcc-buffer
   #:dcc-filename #:dcc-filesize #:dcc-bytes-transferred
   #:dcc-type-string #:dcc-status-string
   #:dcc-accept #:dcc-reject #:dcc-close
   #:dcc-chat-send
   #:find-dcc-connection-for-buffer
   #:dcc-initiate-chat #:dcc-initiate-send
   #:dcc-handle-offer
   #:ip-integer-to-string #:ip-string-to-integer
   #:get-local-ip #:set-dcc-ip))

(defpackage #:clatter.app
  (:use #:cl)
  (:import-from #:clatter.core.model #:make-app #:app-buffers #:make-buffer)
  (:import-from #:clatter.core.config #:load-config #:save-config #:config-networks
                #:make-network-config #:default-libera-config #:add-network-config)
  (:import-from #:clatter.core.commands #:*current-connection*)
  (:import-from #:clatter.ui.tui #:run-tui)
  (:import-from #:clatter.net.irc #:start-irc-connection)
  (:export #:start))
