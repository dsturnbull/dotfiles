require 'rubygems'
require 'hirb'
require 'wirble'
require 'pp'
require 'irb/completion'
require 'irb/ext/save-history'

ARGV.concat [ "--readline", "--prompt-mode", "simple" ]

module IRB
  module HistorySavingAbility
    def HistorySavingAbility.create_finalizer
      at_exit do
        if num = IRB.conf[:SAVE_HISTORY] and (num = num.to_i) > 0
          if hf = IRB.conf[:HISTORY_FILE]
            file = File.expand_path(hf)
          end
          file = IRB.rc_file("_history") unless file
          open(file, 'w' ) do |f|
            hist = HISTORY.to_a
            f.puts(hist[-num..-1] || hist)
          end
        end
      end
    end

    def HistorySavingAbility.extended(obj)
      HistorySavingAbility.create_finalizer

      obj.load_history
      obj
    end
  end
end
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-save-history"

include Hirb
Hirb::View.enable

Wirble.init
Wirble.colorize

class Object
  def local_methods
    (methods - Object.instance_methods).sort
  end
end

if ENV.include?('RAILS_ENV') && !Object.const_defined?('RAILS_DEFAULT_LOGGER')
  require 'logger'
  RAILS_DEFAULT_LOGGER = Logger.new(STDOUT)
end
