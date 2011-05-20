require 'rubygems'
require 'hirb'
require 'wirble'
require 'pp'
require 'irb/completion'
require 'irb/ext/save-history'

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
