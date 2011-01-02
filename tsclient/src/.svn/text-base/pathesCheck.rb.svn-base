#!/usr/local/bin/ruby
# coding: utf-8
require 'fileutils'
STYLES = "styles.css"
content = IO.read(STYLES)
content.scan(/"([^"]+)"/).each do |path|
	path = path[0]
	if path.include?( "assets" ) then
		if path[0] == "/" then
			path = path[1..-1]
		else
			content.gsub!("\"#{path}\"", "\"/#{path}\"")
		end 
		if !FileTest.exists?(path) then
			p "not exists: " + path
		end
	end
end
file = File.new(STYLES, 'w+')
file.puts( content )
file.close
