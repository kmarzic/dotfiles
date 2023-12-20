#!/bin/bash

function __get_latest()
{
    mkdir -p ~/.local/share/fonts

    ## ------------------------------------------------------------------------
    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/DejaVuSansMono
    ## ------------------------------------------------------------------------
    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/DejaVuSansMono/Bold-Italic/complete
    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/DejaVuSansMono/Bold-Italic/complete"
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFont-BoldOblique.ttf"            https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Bold-Italic/DejaVuSansMNerdFont-BoldOblique.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFontMono-BoldOblique.ttf"        https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Bold-Italic/DejaVuSansMNerdFontMono-BoldOblique.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFontPropo-BoldOblique.ttf"       https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Bold-Italic/DejaVuSansMNerdFontPropo-BoldOblique.ttf?raw=true

    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/DejaVuSansMono/Bold
    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/DejaVuSansMono/Bold"
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFont-Bold.ttf"                   https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Bold/DejaVuSansMNerdFont-Bold.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFontMono-Bold.ttf"               https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Bold/DejaVuSansMNerdFontMono-Bold.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFontPropo-Bold.ttf"              https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Bold/DejaVuSansMNerdFontPropo-Bold.ttf?raw=true

    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/DejaVuSansMono/Italic
    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/DejaVuSansMono/Italic"
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFont-Oblique.ttf"                https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Italic/DejaVuSansMNerdFont-Oblique.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFontMono-Oblique.ttf"            https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Italic/DejaVuSansMNerdFontMono-Oblique.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFontPropo-Oblique.ttf"           https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Italic/DejaVuSansMNerdFontPropo-Oblique.ttf?raw=true 

    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/DejaVuSansMono/Regular
    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/DejaVuSansMono/Regular"
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFont-Regular.ttf"                https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Regular/DejaVuSansMNerdFont-Regular.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFontMono-Regular.ttf"            https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Regular/DejaVuSansMNerdFontMono-Regular.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "DejaVuSansMNerdFontPropo-Regular.ttf"           https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/DejaVuSansMono/Regular/DejaVuSansMNerdFontPropo-Regular.ttf?raw=true

    ## ------------------------------------------------------------------------
    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/Inconsolata
    ## ------------------------------------------------------------------------
    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/Inconsolata"
    cd ~/.local/share/fonts && curl -fLo "InconsolataNerdFont-Bold.ttf"                   https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Inconsolata/InconsolataNerdFont-Bold.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "InconsolataNerdFont-Regular.ttf"                https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Inconsolata/InconsolataNerdFont-Regular.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "InconsolataNerdFontMono-Bold.ttf"               https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Inconsolata/InconsolataNerdFontMono-Bold.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "InconsolataNerdFontMono-Regular.ttf"            https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Inconsolata/InconsolataNerdFontMono-Regular.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "InconsolataNerdFontPropo-Bold.ttf"              https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Inconsolata/InconsolataNerdFontPropo-Bold.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "InconsolataNerdFontPropo-Regular.ttf"           https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Inconsolata/InconsolataNerdFontPropo-Regular.ttf?raw=true

    ## ------------------------------------------------------------------------
    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/UbuntuMono
    ## ------------------------------------------------------------------------
    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/UbuntuMono/Bold-Italic
    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/UbuntuMono/Bold-Italic"
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFont-BoldItalic.ttf"              https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Bold-Italic/UbuntuMonoNerdFont-BoldItalic.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFontMono-BoldItalic.ttf"          https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Bold-Italic/UbuntuMonoNerdFontMono-BoldItalic.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFontPropo-BoldItalic.ttf"         https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Bold-Italic/UbuntuMonoNerdFontPropo-BoldItalic.ttf?raw=true

    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/UbuntuMono/Bold
    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/UbuntuMono/Bold"
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFont-Bold.ttf"                    https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Bold/UbuntuMonoNerdFont-Bold.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFontMono-Bold.ttf"                https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Bold/UbuntuMonoNerdFontMono-Bold.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFontPropo-Bold.ttf"               https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Bold/UbuntuMonoNerdFontPropo-Bold.ttf?raw=tru

    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/UbuntuMono/Regular-Italic
    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/UbuntuMono/Regular-Italic"
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFont-Italic.ttf"                  https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Regular-Italic/UbuntuMonoNerdFont-Italic.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFontMono-Italic.ttf"              https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Regular-Italic/UbuntuMonoNerdFontMono-Italic.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFontPropo-Italic.ttf"             https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Regular-Italic/UbuntuMonoNerdFontPropo-Italic.ttf?raw=true

    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/UbuntuMono/Regular
    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/UbuntuMono/Regular"
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFont-Regular.ttf"                 https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Regular/UbuntuMonoNerdFont-Regular.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFontMono-Regular.ttf"             https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Regular/UbuntuMonoNerdFontMono-Regular.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "UbuntuMonoNerdFontPropo-Regular.ttf"            https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/UbuntuMono/Regular/UbuntuMonoNerdFontPropo-Regular.ttf?raw=true

    ## ------------------------------------------------------------------------
    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/SourceCodePro
    ## ------------------------------------------------------------------------
    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/SourceCodePro/Regular"
    cd ~/.local/share/fonts && curl -fLo "SauceCodeProNerdFont-Regular.ttf"               https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/SourceCodePro/Regular/SauceCodeProNerdFont-Regular.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "SauceCodeProNerdFontMono-Regular.ttf"           https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/SourceCodePro/Regular/SauceCodeProNerdFontPropo-Regular.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "SauceCodeProNerdFontPropo-Regular.ttf"          https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/SourceCodePro/Regular/SauceCodeProNerdFontPropo-Regular.ttf?raw=true

    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/SourceCodePro/Bold"
    cd ~/.local/share/fonts && curl -fLo "SauceCodeProNerdFont-Bold.ttf"                  https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/SourceCodePro/Bold/SauceCodeProNerdFont-Bold.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "SauceCodeProNerdFontMono-Bold.ttf"              https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/SourceCodePro/Bold/SauceCodeProNerdFontMono-Bold.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "SauceCodeProNerdFontPropo-Bold.ttf"             https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/SourceCodePro/Bold/SauceCodeProNerdFontPropo-Bold.ttf?raw=true

    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/SourceCodePro/Italic"
    cd ~/.local/share/fonts && curl -fLo "SauceCodeProNerdFont-Italic.ttf"                https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/SourceCodePro/Italic/SauceCodeProNerdFont-Italic.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "SauceCodeProNerdFontMono-Italic.ttf"            https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/SourceCodePro/Italic/SauceCodeProNerdFontMono-Italic.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "SauceCodeProNerdFontPropo-Italic.ttf"           https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/SourceCodePro/Italic/SauceCodeProNerdFontPropo-Italic.ttf?raw=true

    ## ------------------------------------------------------------------------
    ## https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/Hack
    ## ------------------------------------------------------------------------
    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/Hack/Bold"
    cd ~/.local/share/fonts && curl -fLo "HackNerdFont-Bold.ttf"                         https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/Bold/HackNerdFont-Bold.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "HackNerdFontMono-Bold.ttf"                     https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/Bold/HackNerdFontMono-Bold.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "HackNerdFontPropo-Bold.ttf"                    https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/Bold/HackNerdFontPropo-Bold.ttf?raw=true

    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/Hack/BoldItalic"
    cd ~/.local/share/fonts && curl -fLo "HackNerdFont-BoldItalic.ttf"                   https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/BoldItalic/HackNerdFont-BoldItalic.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "HackNerdFontMono-BoldItalic.ttf"               https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/BoldItalic/HackNerdFontMono-BoldItalic.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "HackNerdFontPropo-BoldItalic.ttf"              https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/BoldItalic/HackNerdFontPropo-BoldItalic.ttf?raw=true

    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/Hack/Italic"
    cd ~/.local/share/fonts && curl -fLo "HackNerdFont-Italic.ttf"                       https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/Italic/HackNerdFont-Italic.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "HackNerdFontMono-Italic.ttf"                   https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/Italic/HackNerdFontMono-Italic.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "HackNerdFontPropo-Italic.ttf"                  https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/Italic/HackNerdFontPropo-Italic.ttf?raw=true

    echo ""
    echo "https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/Hack/Regular"
    cd ~/.local/share/fonts && curl -fLo "HackNerdFont-Regular.ttf"                      https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/Regular/HackNerdFont-Regular.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "HackNerdFontMono-Regular.ttf"                  https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/Regular/HackNerdFontMono-Regular.ttf?raw=true
    cd ~/.local/share/fonts && curl -fLo "HackNerdFontPropo-Regular.ttf"                 https://github.com/ryanoasis/nerd-fonts/blob/master/patched-fonts/Hack/Regular/HackNerdFontPropo-Regular.ttf?raw=true

    ## ------------------------------------------------------------------------
    ## https://gitlab.com/timescam/noto-fonts-emoji-apple
    ## ------------------------------------------------------------------------
    echo ""
    echo "https://gitlab.com/timescam/noto-fonts-emoji-apple"
    cd ~/.local/share/fonts && curl -fLo "NotoColorEmoji.ttf"                            https://gitlab.com/timescam/noto-fonts-emoji-apple/-/raw/master/NotoColorEmoji.ttf

    fc-cache -vf
    fc-list
}


function __get_release()
{
    [[ ! -d ~/Downloads ]] && mkdir ~/Downloads
    mkdir -p ~/.local/share/fonts

    #### https://github.com/ryanoasis/nerd-fonts/ - 1.2.0
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v1.2.0/DejaVuSansMono.zip -o ~/Downloads/DejaVuSansMono_v1.2.0.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v1.2.0/BitstreamVeraSansMono.zip -o ~/Downloads/BitstreamVeraSansMono_v1.2.0.zip

    #### https://github.com/ryanoasis/nerd-fonts/ - 2.0.0
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.0.0/DejaVuSansMono.zip -o ~/Downloads/DejaVuSansMono_v2.0.0.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.0.0/BitstreamVeraSansMono.zip -o ~/Downloads/BitstreamVeraSansMono_v2.0.0.zip

    #### https://github.com/ryanoasis/nerd-fonts/ - 2.1.0
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/DejaVuSansMono.zip -o ~/Downloads/DejaVuSansMono_v2.1.0.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/BitstreamVeraSansMono.zip -o ~/Downloads/BitstreamVeraSansMono_v2.1.0.zip

    #### https://github.com/ryanoasis/nerd-fonts/ - 2.2.1
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.2.1/DejaVuSansMono.zip -o ~/Downloads/DejaVuSansMono_v2.2.1.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.2.1/BitstreamVeraSansMono.zip -o ~/Downloads/BitstreamVeraSansMono_v2.2.1.zip

    #### https://github.com/ryanoasis/nerd-fonts/ - 2.2.2
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.2.2/DejaVuSansMono.zip -o ~/Downloads/DejaVuSansMono_v2.2.2.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.2.2/BitstreamVeraSansMono.zip -o ~/Downloads/BitstreamVeraSansMono_v2.2.2.zip

    #### https://github.com/ryanoasis/nerd-fonts/ - 2.3.3
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.3/DejaVuSansMono.zip -o ~/Downloads/DejaVuSansMono_v2.3.3.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.3/BitstreamVeraSansMono.zip -o ~/Downloads/BitstreamVeraSansMono_v2.3.3.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.3/Inconsolata.zip -o ~/Downloads/Inconsolata_v2.3.3.zip

    #### https://github.com/ryanoasis/nerd-fonts/ - 3.0.0
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.0/DejaVuSansMono.zip -o ~/Downloads/DejaVuSansMono_v3.0.0.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.0/Inconsolata.zip -o ~/Downloads/Inconsolata_v3.0.0.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.0/UbuntuMono.zip -o ~/Downloads/UbuntuMono_v3.0.0.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.0/SourceCodePro.zip -o ~/Downloads/SourceCodePro_v3.0.0.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.0/Hack.zip -o ~/Downloads/Hack_v3.0.0.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.0/BitstreamVeraSansMono.zip -o ~/Downloads/BitstreamVeraSansMono_v3.0.0.zip

    #### https://github.com/ryanoasis/nerd-fonts/ - 3.0.1
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.1/DejaVuSansMono.zip -o ~/Downloads/DejaVuSansMono_v3.0.1.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.1/Inconsolata.zip -o ~/Downloads/Inconsolata_v3.0.1.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.1/UbuntuMono.zip -o ~/Downloads/UbuntuMono_v3.0.1.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.1/SourceCodePro.zip -o ~/Downloads/SourceCodePro_v3.0.1.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.1/Hack.zip -o ~/Downloads/Hack_v3.0.1.zip
    # curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.1/BitstreamVeraSansMono.zip -o ~/Downloads/BitstreamVeraSansMono_v3.0.1.zip

    #### https://github.com/ryanoasis/nerd-fonts/ - 3.1.1
    curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/DejaVuSansMono.zip -o ~/Downloads/DejaVuSansMono_v3.1.1.zip
    curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/Inconsolata.zip -o ~/Downloads/Inconsolata_v3.1.1.zip
    curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/UbuntuMono.zip -o ~/Downloads/UbuntuMono_v3.1.1.zip
    curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/SourceCodePro.zip -o ~/Downloads/SourceCodePro_v3.1.1.zip
    curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/Hack.zip -o ~/Downloads/Hack_v3.1.1.zip
    curl -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/BitstreamVeraSansMono.zip -o ~/Downloads/BitstreamVeraSansMono_v3.1.1.zip

    ## https://gitlab.com/timescam/noto-fonts-emoji-apple
    cd ~/.local/share/fonts && curl -fLo "NotoColorEmoji.ttf"                            https://gitlab.com/timescam/noto-fonts-emoji-apple/-/raw/master/NotoColorEmoji.ttf

    cd ~/Downloads
    mv Deja*.ttf ~/.local/share/fonts
    rm Deja*
    mv Inconsolata*.ttf ~/.local/share/fonts
    rm Inconsolata*
    mv UbuntuMono*.ttf ~/.local/share/fonts
    rm UbuntuMono*
    mv SourceCode*.ttf ~/.local/share/fonts
    rm SourceCode*
    mv Hack*.ttf ~/.local/share/fonts
    rm Hack*
    mv Bitstream*.ttf ~/.local/share/fonts
    rm Bitstream*

    fc-cache -vf
    fc-list
}

## MAIN
__get_latest
# __get_release

#### END
