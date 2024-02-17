module Consts(module Consts) where
    
    header :: String
    header = "<!DOCTYPE html>\n\
\<html lang=\"en\">\n\
\<head>\n\
\    <style>\n\
\        tr,\n\
\        td {\n\
\            border: 1px solid grey;\n\
\            width: 80px;\n\
\           height: 80px;\n\
\            text-align: center;\n\
\        }\n\
\    </style>\n\
\</head>\n\
\<body> "

    footer_base :: String
    footer_base = "</html>"

    footer :: String
    footer = "    <h2 class=\"step\">Write your step: </h2>\n\
\    <form class=\"myForm\" method=\"POST\" action=\"/\">\n\
\        <input name=\"cell\">\n\
\        <button>Send</button>\n\
\    </form>\n\
\</html>"
