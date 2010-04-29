require 'irb/completion'
require 'irb/ext/save-history'
require 'open-uri'
require 'json'
require 'cgi'

ANSI_YELLOW   = "\e[33m"
ANSI_PURPLE   = "\033[35m"
ANSI_RESET    = "\033[0m"
ANSI_BOLD     = "\e[1m"

begin
  require 'wirble'

  Wirble.init
  Wirble.colorize
rescue LoadError => err
  warn "Couldn't load Wirble: #{err}"
end

ARGV.concat [ "--readline", "--prompt-mode" ]
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"
IRB.conf[:AUTO_INDENT] = true

IRB.conf[:PROMPT][:CLOUD] = { 
  :PROMPT_I => "#{ANSI_PURPLE}%N>#{ANSI_RESET}\n ",   # normal prompt
  :PROMPT_S => "#{ANSI_PURPLE}%N%l#{ANSI_RESET}\n ",  # prompt for continuing strings
  :PROMPT_C => "#{ANSI_PURPLE}%N*#{ANSI_RESET}\n ",   # prompt for continuing statement
  :PROMPT_N => "#{ANSI_PURPLE}%N*#{ANSI_RESET}\n ",   # "
  :RETURN   => "#{ANSI_PURPLE}=>#{ANSI_RESET}#{ANSI_YELLOW} %s#{ANSI_RESET}\n"
}
IRB.conf[:PROMPT_MODE] = :SIMPLE


