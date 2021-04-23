-- LUA filter for Pandoc
-- Replace all relative links in Markdown document by absolute URL for use in Gitlab pages.
-- Relative URL "misc/CompilerConstruction.pdf" is replaced by
-- "https://gitlab.com/pmetras/nim0/-/blob/master/misc/CompilerConstruction.pdf"
-- when generating static pages.
-- The absolute base URL is taken from the "webroot" metadata element that must
-- be defined in the Markdown document.

-- The value that will be retrieved from the Metadata of the Markdown document
local webroot = nil

-- Set webroot from document metadata "webroot" attribute.
function Meta(meta)
  webroot = meta.webroot[1]
  webroot = webroot["text"]
end

-- Fix relative URL prepending webroot value when defined.
function fix_link(url)
  if webroot == nil then
    return url
  end

  if url.sub(1, 4) ~= "http" then
    return webroot .. url
  end
  return url
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