#!/bin/bash

function show_help {
   echo "Usage: buildsite.sh [OPTION]..."
    echo "Knit posts, rebuild jekyll blog <chepec>"
    echo ""
    echo "-c   convert all _Rmd/*.Rmd files to _posts/*.md (does not overwrite existing md)"
    echo "-o   overwrite existing *.md files"
   echo "-f   convert '~/Desktop/research/CS_misc/GitHub/mypage/_Rmd/<filename>.Rmd' to md in _posts"
    echo "-b   build the jekyll site"
    echo "-s   send changes to live dir"
    echo "-h   show this help"
}

rmdfile=""
bashpwd=$(pwd)

# initialise flags here
# convert new *.Rmd => *.md?
convert=true
# convert even if *.md exists?
overwrite_md=false
# convert a specific Rmd file?
convert_file=false
# build jekyll site?
buildsite=false
# make changes live?
send_live=false

if [ $# -eq 0 ] ; then
   # no args at all? show usage
   show_help
   exit 0
fi

# reset OPTINDEX in case getopts has been used previously in the shell
OPTIND=1
while getopts "h?cf:obs" opt; do
    case $opt in
        c)
            # just a flag, no options
            convert=true
            ;;
      f)
         # has a mandatory option, filename
         convert_file=true
         rmdfile=$OPTARG
         ;;
        o)
            # just a flag, no options
            overwrite_md=true
            ;;
        b)
            # just a flag, no options
            buildsite=true
            ;;
      s)
            # just a flag, no options
            send_live=true
            ;;
        h|\?)
            show_help
            exit 0
    esac
done
shift $((OPTIND-1))

#### 1. convert/overwrite *.Rmd to *.md
# uses the function KnitPost() from the R-script render_post.R
if [ "$convert" = true ] ; then
    echo ">>>> jc: Converting RMarkdown to Markdown (overwrite = $overwrite_md)"
    if [ "$overwrite_md" = true ] ; then
        Rscript -e "source('~/Desktop/research/CS_misc/GitHub/mypage/_Rmd/render_post.R'); KnitPost(overwrite=TRUE)"
    else
        Rscript -e "source('~/Desktop/research/CS_misc/GitHub/mypage/_Rmd/render_post.R'); KnitPost()"
    fi
fi

#### 2. convert a specific Rmd-file
# uses the function KnitPost() from the R-script render_post.R
if [ "$convert_file" = true ] ; then
   # check that $rmdfile exists
   if [ -f $rmdfile ]; then
      Rscript -e "source('~/Desktop/research/CS_misc/GitHub/mypage/_Rmd/render_post.R'); KnitPost(bashwd='$bashpwd',convert_file='$rmdfile')"
   else
      show_help
      exit 0
   fi
fi

#### 3. build the site
if [ "$buildsite" = true ] ; then
    echo ">>>> jc: Building site to /var/www/blog"
    jekyll build -s ~/jekyll/chepec -d /var/www/blog
fi

#### 4. make changes live (send to public server)
if [ "$send_live" = true ] ; then
    echo ">>>> jc: Sending changes to live server"
    unison jekyll-damietta -auto
fi

exit 0