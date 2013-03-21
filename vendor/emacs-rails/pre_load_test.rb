::TEST_PRE_LOAD_RUNNER=true
require File.expand_path("./test/test_helper")

while a=gets
  b=Time.now.to_f
  Process.wait fork {
    req,args= a.strip.split(':')
    $0=req
    if args
      ARGV[0..-1]=args.split
    else
      ARGV.clear
    end
    puts "#{req} #{args}"
    require req
  }
  puts("\nExit Status #{$?.exitstatus}. Completed in %0.3f seconds" % (Time.now.to_f - b))
  if defined?(Dispatcher)
    Dispatcher.cleanup_application
    Dispatcher.reload_application
  else
    ActionDispatch::Callbacks.new(Proc.new {}, false).call({})
  end
  ActiveRecord::Base.connection.reset!
  ActiveRecord::Base.connection_pool.disconnect!
end
