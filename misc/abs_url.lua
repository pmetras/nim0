-- LUA filter for Pandoc
-- Replace all relative links in Markdown document by absolute URL for use in Gitlab pages.
-- Relative URL "misc/CompilerConstruction.pdf" is replaced by
-- "https://gitlab.com/pmetras/nim0/-/blob/master/misc/CompilerConstruction.pdf"
-- when generating static pages.
-- Relative URL "/doc/OSG.html" is replaced by
-- "https://pmetras.gitlab.io/nim0/doc/OSG.html"
-- The absolute base URLs are taken from the "webroot" and "gitroot" metadata elements that must
-- be defined in the Markdown document.

-- The values that will be retrieved from the Metadata of the Markdown document
local webroot = nil
local gitroot = nil

-- Set webroot and gitroot from document metadata attributes.
function Meta(meta)
  webroot = meta.webroot[1]
  webroot = webroot["text"]
  gitroot = meta.gitroot[1]
  gitroot = gitroot["text"]
end

-- Fix relative URL prepending base value when defined.
function fix_link(url)
  if string.sub(url, 1, 4) == "http" then
    return url
  elseif string.sub(url, 1, 1) == "/" then
    return webroot .. url
  else
    return gitroot .. url
  end
end

-- Check links URL <a ref=...>
function Link(link)
  link.target = fix_link(link.target)
  return link
end

-- Check image URLs <img src=...>
function Image(img)
  img.src = fix_link(img.src)
  return img
end

-- Apply filter in that order:
-- 1. Parse metadata
-- 2. Process links and images
return {{Meta = Meta}, {Link = Link, Image = Image}}