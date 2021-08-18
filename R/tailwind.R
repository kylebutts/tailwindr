#' TailwindCSS in Rmd documents
#'
#' @details
#'  # About Tailwind
#'
#'  TailwindCSS is a utility-based design framework that makes designing simple.
#'  It follows the `atomic css` framework that says 1 class = 1 task. For example,
#'  the `py-4` class adds `4rem` of padding in the y direction. `text-gray-500`
#'  sets the text to the color gray-500 which is part of Tailwind's beautiful
#'  default color scheme. `font-semibold` sets the font weight to 600.
#'
#'  For responsive design, you can add prefixes to class names. For example
#'  `px-2 md:px-4` increases the x-direction padding on medium or larger screens.
#'  There are a ton of cool features, for example
#'  `bg-gradient-to-r from-yellow-400 via-red-500 to-pink-500` crates a gradient
#'  background from yellow-400 to pink-500 passing via red-500 (see it here:
#'  \url{https://tailwindcss.com/docs/background-image#linear-gradients}).
#'
#'  For complete documentation, see \url{https://tailwindcss.com/docs/}
#'
#'  # Tailwind Typography
#'
#'  Uses Tailwind Typography. This is an opinionated css framework that creates
#'  beautiful text-based documents that work incredibly well for .Rmd documents.
#'  For more information visit
#'  \url{https://github.com/tailwindlabs/tailwindcss-typography}.
#'
#'  # `self_contained` Option
#'  There are two option for compiling CSS:
#'
#'  1. `self_contained: true`
#'    Requires Node (npm) to be installed on system. This option will post_process
#'    the knitted document and create the custom tailwind css. For example,
#'    css classes with the `@apply` tag will be compiled and tailwind will be loaded.
#'
#'    The parameter `slim_css` uses PostCSS to only include the css classes that
#'    appear in the final html document. This is great for keeping files very
#'    small, but bad if you are trying to edit through chrome or firefox for example.
#'    I recommend putting `slim_css: false` into the yaml while developing and
#'    `slim_css: true` when ready to finish.
#'
#'    It is possible to pass a custom tailwind configuration using the standard
#'    format (\url{https://tailwindcss.com/docs/configuration}). This will be
#'    passed to the node script as required.
#'
#'  2. `self_contained: false`
#'    Does not require node (npm) to be installed. Instead of post_processing the
#'    css, instead this option will use Tailwind Just-in-time Compiler which allows
#'    css to be generated as needed in the document. This requires internet
#'    connection, though. This is great for opening documents and trying out
#'    classes with Chrome/Firefox Developer Tools. For more infomration on the
#'    Just-in-time feature, see
#'    \url{https://beyondco.de/blog/tailwind-jit-compiler-via-cdn}.
#'
#'    Tailwind configuration is also possible in this mode. However, it requires
#'    a non-standard config file. See `tailwindr::use_tailwind` for details.
#'
#'  # Custom CSS
#'
#'  Custom css can use the `@apply` directives that come with tailwind to easily
#'  compile set of classes. See
#'  \url{https://tailwindcss.com/docs/functions-and-directives#apply} for
#'  more details. It just **has** to be passed to the use_tailwind function if
#'  you want to use the `@apply` directive.
#'
#'  ## Example css
#'
#'  For example, we could create a custom button with class btn. And create a
#'  blue and red variant
#'
#'  ```{css}
#'    /* File: style.css */
#'
#'    .btn {
#'      @apply font-bold py-2 px-4 rounded;
#'    }
#'    .btn-blue {
#'      @apply bg-blue-500 hover:bg-blue-700 text-white;
#'    }
#'    .btn-red {
#'      @apply bg-red-500 hover:bg-red-700 text-white;
#'    }
#'  ```
#'
#'   Custom css is possible by passing objects to the `css` yaml parameter.
#'   Importantly, you can use the `@apply` directives that come with tailwind
#'   to easily compile set of classes. See
#'   \url{https://tailwindcss.com/docs/functions-and-directives#apply}
#'   for more details.
#'
#'
#'
#'
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "tango", "pygments", "kate", "monochrome", "espresso", "zenburn",
#'   "haddock", and "textmate". Pass NULL to prevent syntax highlighting.
#' @param slim_css Whether or not to include entirety of TailwindCSS or not. See
#'   Details for more information.
#' @param self_contained Produce a standalone HTML file with no external
#'   dependencies, using data URIs to incorporate the contents of linked scripts,
#'   stylesheets, images, and videos. Note that if true, this requires Node (npm)
#'   to be installed on the system
#' @param css CSS files to include. See Details for more details on using
#'   `@apply`.
#' @param tailwind_config Custom tailwind config file.
#'   If `self_contained` is true, this is the standard config format described
#'   by \url{https://tailwindcss.com/docs/configuration}
#'   If `self_contained` is false, then you should use a config for the JIT CDN
#'   version following the details in `tailwindr::use_tailwind`.
#' @param clean_supporting Logical. Whether or not to clear supporting files.
#'   Default is TRUE.
#' @param template Pandoc template to use for rendering. Pass `NULL` to use
#'   built-in tailwand template. See `example` folder in source code for example
#'   of using Tailwind CSS in template.
#' @param ... Additional arguments passed to `rmarkdown::html_document` base_format.
#'
#' @export
#'
tailwind = function(highlight = "zenburn", css = NULL, tailwind_config = NULL, slim_css = FALSE, self_contained = TRUE, clean_supporting = TRUE, template = NULL, ...) {

    # Store output_dir
    output_dir <- ""
    files_dir <- ""
    compiled_css = c("tailwind_compiled.css")

    # Copy css files (for manual compression later)
    pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
        # copy supplied output_dir (for use in post-processor)
        files_dir <<- files_dir
        output_dir <<- output_dir

        invisible(NULL)
    }

    # Post processor
    # PurgeCSS
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {

        output_str <- xfun::read_utf8(output_file)
        # output_dir from pre_processor is already defined

        # Input css
        css_idx <- which(stringr::str_detect(output_str, "<!-- css goes here -->"))
        # If no input css, put before </head>
        if(length(css_idx) == 0) {
            css_idx <- which(stringr::str_detect(output_str, "</head>")) - 1
        }


        if(self_contained) {
            # tailwind_prose.css
            # this ensures tailwind is loaded
            if(!file.exists(file.path(output_dir, "tailwind_prose.css"))) {
                cat('
                @tailwind base;
                @tailwind components;
                @tailwind utilities;
                ', file = file.path(output_dir, "tailwind_prose.css"))
            }

            # tailwind.config.js
            # custom config
            if(!is.null(tailwind_config)) {
                file.copy(
                    from = file.path(files_dir, tailwind_config),
                    to = file.path(output_dir, "tailwind.config.js"),
                    overwrite = TRUE
                )
            }
            # default config
            else {
                if(slim_css) {
                    if(!file.exists(file.path(output_dir, "tailwind.config.js"))) {
                        cat(paste0('module.exports = {
                      purge: {
                        enabled: true,
                        content: ["', output_file, '"]
                      },
                      darkMode: false,
                      theme: {
                        extend: {},
                      },
                      variants: {
                        extend: {},
                      },
                      plugins: [
                        require("@tailwindcss/typography")
                      ],
                    }'), file = file.path(output_dir, "tailwind.config.js"))
                    }
                } else {
                    if(!file.exists(file.path(output_dir, "tailwind.config.js"))) {
                        cat(paste0('module.exports = {
                      purge: {
                      },
                      darkMode: false,
                      theme: {
                        extend: {},
                      },
                      variants: {
                        extend: {},
                      },
                      plugins: [
                        require("@tailwindcss/typography")
                      ],
                    }'), file = file.path(output_dir, "tailwind.config.js"))
                    }
                }
            }

            # postcss.config.js
            if(!file.exists(file.path(output_dir, "postcss.config.js"))) {
                cat('// postcss.config.js
                module.exports = {
                    plugins: {
                      tailwindcss: { },
                      autoprefixer: {},
                    }
              }', file = file.path(output_dir, "postcss.config.js"))
            }

            command <- paste0(
                # change directory to output directory
                "cd ", output_dir, ";",
                # install modules
                "npm list tailwindcss || npm install tailwindcss@latest;",
                "npm list postcss || npm install postcss@latest;",
                "npm list autoprefixer || npm install autoprefixer@latest;",
                "npm list @tailwindcss/typography || npm install @tailwindcss/typography;",
                # Run postcss on css files
                "cat ", file.path(output_dir, "tailwind_prose.css"), " ", paste(css, collapse = " "), " | ",
                "postcss > tailwind_compiled.css"
            )
            system(command)

            # Create URI of style
            css_uri <- htmltools::tags$link(
                href = xfun::base64_uri(file.path(output_dir, "tailwind_compiled.css")),
                rel = "stylesheet"
            )

            if(clean_supporting) {
                file.remove(
                    file.path(output_dir, "postcss.config.js"),
                    file.path(output_dir, "tailwind_prose.css"),
                    file.path(output_dir, "tailwind_compiled.css")
                )
                if(file.path(files_dir, "tailwind.config.js") != file.path(output_dir, "tailwind.config.js")) {
                    file.remove(file.path(output_dir, "tailwind.config.js"))
                }
            }


            output_str <- append(output_str,
                                 as.character(css_uri),
                                 after = css_idx)
        }
        if(!self_contained) {
            # Default config
            if(!is.null(tailwind_config)) {
                tailwind_config = system.file("example_config", "forum_and_typography.js", package = "tailwindr")
            }

            tailwind_str <- tailwindr::use_tailwind(
                css = css,
                tailwindConfig = tailwind_config
            )

            output_str <- append(output_str,
                                 as.character(tailwind_str),
                                 after = css_idx)
        }

        xfun::write_utf8(output_str, output_file)
        output_file
    }


    # Allow custom templates
    if(!is.null(template)) template <- system.file(files_dir, template)
    if(is.null(template)) template <- system.file("templates/tailwind/tailwind.html", package = "tailwindr")

    # https://github.com/rstudio/rmarkdown/blob/0af6b3556adf6e393b2da23c66c695724ea7bd2d/R/html_notebook.R
    # generate actual format
    base_format <- rmarkdown::html_document(
        fig_width = 8, fig_height = 4,
        theme = NULL, highlight = highlight, css = compiled_css,
        self_contained = self_contained,
        template = template,
        ...
    )

    rmarkdown::output_format(
        knitr = tailwind_knitr_options(),
        pandoc = NULL,
        base_format =  base_format,
        pre_processor = pre_processor,
        post_processor = post_processor,
        clean_supporting = FALSE
    )
}


# From https://github.com/rstudio/rmarkdown/blob/0af6b3556adf6e393b2da23c66c695724ea7bd2d/R/html_notebook.R
# This uses the default knit hooks plus a custom one for figures
#
tailwind_knitr_options <- function() {

    # save original hooks from knitr and restore after we've stored requisite
    # hooks for our output format
    # This prevents permanently changing knitr hooks outside of this output_format
    saved_hooks <- get_knitr_hook_list()
    on.exit(set_knitr_hook_list(saved_hooks), add = TRUE)


    # use 'render_markdown()' to set defatul hooks
    # note: this is why we had to do the above
    knitr::render_markdown()

    # store original hooks and annotate in format
    knit_hooks <- knitr::knit_hooks$get()

    # Update knit_hook for "plot"
    knit_hooks$plot = function(x, options) {
        cap  <- options$fig.cap  # figure caption
        tags <- htmltools::tags

        style <- paste(
            c(sprintf('width: %s', options$out.width),
              sprintf('height: %s', options$out.height)),
            collapse = '; '
        )

        if(options$dev == "svg" | options$dev == "svglite") {
            as.character(tags$figure(
                tags$div(style = style,
                     # cat svg to HTML
                     htmltools::HTML(paste(readLines(x), collapse = ''))
                ),
                tags$figcaption(cap)
            ))
        } else {
            as.character(tags$figure(
                tags$img(src = x, alt = cap, style = style),
                tags$figcaption(cap)
            ))
        }
    }


    # return as knitr options
    rmarkdown::knitr_options(knit_hooks = knit_hooks)
}

# From https://github.com/rstudio/rmarkdown/blob/0af6b3556adf6e393b2da23c66c695724ea7bd2d/R/util.R
get_knitr_hook_list <- function(hook_names = NULL) {
    if (is.null(hook_names))
        hook_names <- c("knit_hooks", "opts_chunk", "opts_hooks", "opts_knit")
    knitr_ns <- asNamespace("knitr")
    hook_list <- lapply(hook_names, function(hook_name) {
        hooks <- get(hook_name, envir = knitr_ns, inherits = FALSE)
        hooks$get()
    })
    names(hook_list) <- hook_names
    hook_list
}
set_knitr_hook_list <- function(hook_list) {
    knitr_ns <- asNamespace("knitr")
    enumerate(hook_list, function(hook_name, hook_value) {
        hook <- get(hook_name, envir = knitr_ns, inherits = FALSE)
        hook$set(hook_value)
    })
}
enumerate <- function(data, f, ...) {
    lapply(seq_along(data), function(i) {
        f(names(data)[[i]], data[[i]], ...)
    })
}

# if LHS is NULL, return the RHS
`%n%` = function(x, y) if (is.null(x)) y else x
