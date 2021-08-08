#' TailwindCSS with Tailwind Typography in Rmd documents
#'
#' @details
#'   Requires Node (npm) to be installed on system.
#'
#'   Uses Tailwind Typography. For more information visit
#'   \url{https://github.com/tailwindlabs/tailwindcss-typography}.
#'
#'   The parameter `slim_css` uses PostCSS to only include the css classes that
#'   appear in the final html document. This is great for keeping files very
#'   small, but bad if you are trying to edit through chrome or firefox for example.
#'   I recommend putting `slim_css: false` into the yaml while developing and
#'   `slim_css: true` when ready to finish.
#'
#'   Custom css is possible by passing objects to the `css` yaml parameter.
#'   Importantly, you can use the `@apply` directives that come with tailwind
#'   to easily compile set of classes. See
#'   \url{https://tailwindcss.com/docs/functions-and-directives#apply}
#'   for more details.
#'
#'
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "tango", "pygments", "kate", "monochrome", "espresso", "zenburn",
#'   "haddock", and "textmate". Pass NULL to prevent syntax highlighting.
#' @param slim_css Whether or not to include entirety of TailwindCSS or not. See
#'   Details for more information.
#' @param self_contained Produce a standalone HTML file with no external
#'   dependencies, using data: URIs to incorporate the contents of linked scripts,
#'   stylesheets, images, and videos. Note that even for self contained documents
#'   MathJax is still loaded externally (this is necessary because of its size).
#'   *Important:* Everything will be self_contained except for the compiled css
#'   (for now!)
#' @param css CSS files to include. See Details for moremore details on using
#'   `@apply`.
#' @param clean_supporting Logical. Whether or not to clear supporting files.
#'   Default is TRUE.
#' @param template Pandoc template to use for rendering. Pass `NULL` to use
#'   built-in tailwand template. See `example` folder in source code for example
#'   of using Tailwind CSS in template. Note you should use
#'   `<article class="prose">` to use Tailwind Typography!
#'
#' @export
#'
tailwind_prose = function(highlight = "zenburn", slim_css = FALSE, self_contained = TRUE, css = NULL, clean_supporting = TRUE, template = NULL, ...) {

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

        # tailwind_prose.css
        if(!file.exists(file.path(output_dir, "tailwind_prose.css"))) {
            cat('
                @tailwind base;
                @tailwind components;
                @tailwind utilities;
                ', file = file.path(output_dir, "tailwind_prose.css"))
        }

        # postcss.config.js
        if(!file.exists(file.path(output_dir, "postcss.config.js"))) {
            cat('// postcss.config.js
                module.exports = {
                    plugins: {
                      tailwindcss: {},
                      autoprefixer: {},
                    }
              }', file = file.path(output_dir, "postcss.config.js"))
        }

        # tailwind.config.js
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


        command <- paste0(
            # change directory to output directory
            "cd ", output_dir, ";",
            # install modules
            "npm install -D tailwindcss@latest postcss@latest autoprefixer@latest @tailwindcss/typography;",
            # Run postcss on css files
            "cat ", file.path(output_dir, "tailwind_prose.css"), " ", paste(css, collapse = " "), " | ",
            "postcss > tailwind_compiled.css"
        )
        system(command)

        if(clean_supporting) {
            file.remove(
                file.path(output_dir, "tailwind.config.js"),
                file.path(output_dir, "postcss.config.js"),
                file.path(output_dir, "tailwind_prose.css")
            )
        }

        # Input css
        css_idx <- which(stringr::str_detect(output_str, "<!-- css goes here -->"))
        # If no input css, put before </head>
        if(length(css_idx) == 0) {
            css_idx <- which(stringr::str_detect(output_str, "</head>")) - 1
        }

        output_str <- append(output_str,
               "<link rel='stylesheet' href='tailwind_compiled.css' type='text/css'>",
               after = css_idx)

        xfun::write_utf8(output_str, output_file)
        output_file
    }

    # Allow custom templates
    if(is.null(template)) template <- system.file("templates/tailwind_prose/tailwind_prose.html", package = "tailwindr")

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
        knitr = tailwind_prose_knitr_options(),
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
tailwind_prose_knitr_options <- function() {

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
