begin
  require 'rubygems'
  require 'hirb'
  require 'wirble'
  require 'pp'
  require 'irb/completion'
  require 'irb/ext/save-history'

  IRB.conf[:SAVE_HISTORY] = 200
  IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-history"

  ARGV.concat [ "--readline", "--prompt-mode", "simple" ]

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

rescue Exception => e
  puts e
end
