module Jekyll
  class LiterateHaskellConverter < Converter
    CODE_BEGIN = "\n{% highlight haskell %}\n"
    CODE_END = "\n{% endhighlight %}\n"

    safe true
    priority :high

    def matches(ext)
      ext =~ /^\.lhs$/i
    end

    def output_ext(ext)
      ".html"
    end

    def convert(content)
      md_content = convert_to_markdown(content)
      md_converter = Jekyll::Converters::Markdown.new(@config)
      md_converter.convert(md_content)
    end

    private
      def convert_to_markdown(content)
        output = []
        code_block = false
        last_line_was_blank = false
        content.split("\n").each do |line|
          haskell_line = line =~ /^> /
          blank_line = line =~ /^\s*$/
          octothorpe_line = line =~ /^\\#.*/
          case
          when haskell_line && code_block
            if last_line_was_blank
              output << ''
            end
            last_line_was_blank = false
            output << (line[2..-1] || '')
          when haskell_line
            code_block = true
            last_line_was_blank = false
            output << CODE_BEGIN
            output << line[2..-1]
          when code_block && blank_line
            if last_line_was_blank
              output << ''
            end
            last_line_was_blank = true
          when code_block
            code_block = false
            last_line_was_blank = false
            output << CODE_END
            output << ''
            if octothorpe_line
              output << line[1..-1]
            else
              output << line
            end
          when octothorpe_line
            output << line[1..-1]
          else
            output << line
          end
        end
        output << CODE_END if code_block
        output.join("\n")
      end
  end
end
