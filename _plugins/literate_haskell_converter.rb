module Jekyll
  class LiterateHaskellConverter < Converter
    CODE_BEGIN = "~~~haskell"
    CODE_END = "~~~"

    safe true
    priority :highest

    def matches(ext)
      ext =~ /^\.lhs$/i
    end

    def output_ext(ext)
      ".html"
    end

    def convert(content)
      md_content = convert_to_markdown(content)

      md_converter = Jekyll::Converters::Markdown::KramdownPygments.new(@config)
#      md_converter = Jekyll::Converters::Markdown.new(@config)
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






# We define the an additional option for the kramdown parser to look for
module Kramdown
  module Options
      define(:kramdown_default_lang, Symbol, nil, <<EOF)
Sets the default language for highlighting code blocks

If no language is set for a code block, the default language is used
instead. The value has to be one of the languages supported by pygments
or nil if no default language should be used.

Default: nil
Used by: PygmentsHtml converter
EOF
  end
end

# This class is a plugin for kramdown, to make it use pygments instead of coderay
# It has nothing to do with Jekyll, it is simply used by the custom converter below
module Kramdown
  module Converter
    class PygmentsHtml < Html

      begin
        require 'pygments'
      rescue LoadError
        STDERR.puts 'You are missing a library required for syntax highlighting. Please run:'
        STDERR.puts '  $ [sudo] gem install pygments'
        raise FatalException.new("Missing dependency: Pygments")
      end

      def convert_codeblock(el, indent)
        attr = el.attr.dup
        lang = extract_code_language!(attr) || @options[:kramdown_default_lang]
        code = pygmentize(el.value, lang)
        code_attr = {}
        code_attr['class'] = "language-#{lang}" if lang
        "#{' '*indent}<div class=\"highlight\"><pre#{html_attributes(attr)}><code#{html_attributes(code_attr)}>#{code}</code></pre></div>\n"
      end

      def convert_codespan(el, indent)
        attr = el.attr.dup
        lang = extract_code_language!(attr) || @options[:kramdown_default_lang]
        code = pygmentize(el.value, lang)
        if lang
          attr['class'] = "highlight"
          if attr.has_key?('class')
            attr['class'] += " language-#{lang}"
          else
            attr['class'] = "language-#{lang}"
          end
        end
        "<code#{html_attributes(attr)}>#{code}</code>"
      end
      
      def pygmentize(code, lang)
        if lang
          Pygments.highlight(code,
            :lexer => lang,
            :options => { :startinline => true, :encoding => 'utf-8', :nowrap => true })
        else
          escape_html(code)
        end
      end
    end
  end
end

# This class is the actual custom Jekyll converter.
class Jekyll::Converters::Markdown::KramdownPygments

  def initialize(config)
    require 'kramdown'
    @config = config
  rescue LoadError
    STDERR.puts 'You are missing a library required for Markdown. Please run:'
    STDERR.puts '  $ [sudo] gem install kramdown'
    raise FatalException.new("Missing dependency: kramdown")
  end

  def convert(content)
    html = Kramdown::Document.new(content, {
        :auto_ids             => @config['kramdown']['auto_ids'],
        :footnote_nr          => @config['kramdown']['footnote_nr'],
        :entity_output        => @config['kramdown']['entity_output'],
        :toc_levels           => @config['kramdown']['toc_levels'],
        :smart_quotes         => @config['kramdown']['smart_quotes'],
        :kramdown_default_lang => @config['kramdown']['default_lang'],
        :input                => @config['kramdown']['input']
    }).to_pygments_html
    return html;
  end
end
