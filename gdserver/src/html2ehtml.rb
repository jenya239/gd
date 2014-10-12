# encoding: utf-8

require 'nokogiri'

def walker node, indent = ''
	if node.name == 'text'
		indent + '"' + node.content.strip.gsub( '"', '\"' ) + '"'
	else
		attrs = '[' + node.attributes.map{ |el| '{' + ( el[1].name.include?( '-' ) ? "'#{ el[1].name }'" : el[1].name ) + ', "' + el[1].value + '"}' }.join( ', ' ) + ']'
		children_for_print = node.children.reject{ |child| child.name == 'text' and child.content.strip.empty? }
		children_string = if children_for_print.size == 0
			'[]'
		elsif children_for_print.size == 1 and children_for_print[0].name == 'text'
			'"' + children_for_print[0].content.strip.gsub( '"', '\"' ) + '"'
		else
			"[\n" + children_for_print.map{ |child| walker( child, "\t#{ indent }" ) }.join( ",\n" ) + "\n" + indent + "]"
		end
		indent + '{' + (node.name == 'div' ? "'div'" : node.name) + ', ' + attrs + ', ' + children_string + '}'
	end
end

doc = Nokogiri::HTML IO.read ARGV[ 0 ]
print walker doc.root.children[ 0 ].children[ 0 ]