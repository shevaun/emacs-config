#!/usr/bin/ruby
line_re=/^\s*#{ARGV[1]}\s+\/(.+)\/\s+do\s*(?:\|([^|]+)|)?/

puts "(list " << Dir[File.join(ARGV[0],'**/*.rb')].map{|f| open(f){|io| io.grep line_re}}.flatten.map { |line|
  md=line_re.match(line)
  base=md[1].sub(/^\^/,'').sub(/\$$/,'')
  if md[2]
    md[2].split(/\s*,\s*/).each_with_index do |arg,index|
      base.sub!(/(?!\\)\((\\\)|[^\)])*\)/,"${#{index+1}:#{arg}}")
    end
  end
  base.gsub('\\','').inspect
}.join(" ") << ")"
