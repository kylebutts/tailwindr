#' TailwindCSS with Shiny
#'
#' @details
#'
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
#'  # Tailwind Just-in-time Compiler
#'
#'  However, the complete set of tailwind css classes is massive (~15mb), so
#'  you don't want to load all of these. That is where Tailwind's new Just in
#'  Time compiling comes in. It will only load the css classes you use, as you
#'  use them. So if your shiny app renders ui dynamically, it will load whenever
#'  the UI is rendered.
#'
#'  This is all possible thanks to the company Beyond Code who created a browser
#'  version of Tailwind Just in Time. See
#'  \url{https://beyondco.de/blog/tailwind-jit-compiler-via-cdn}.
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
#'  # Custom Tailwind Config
#'
#'  Custom configuration of tailwind is also possible. If you know Tailwind,
#'  things will look slightly different. You need to create a variable
#'  `window.tailwindConfig` which will contain the JSON file and you must
#'  call `window.tailwindCSS.refresh();` after creating window.tailwindConfig.
#'  An example is in the `inst/example_config` folder
#'
#'  If you want to use custom modules, for example TailwindTypography, note that
#'  you need to use the browser-version (cdn version) and you have to layout
#'  the config file in a specific way.
#'  `inst/examples/03-modules` in the github repository.
#'
#'  ## Example config with custom color
#'
#'  Creating color scale is easy with
#'  \url{https://javisperez.github.io/tailwindcolorshades/?alice=0990af}
#'
#'  ```{js}
#'    // File: tailwind.config.js
#'
#'    // Define config file
#'    window.tailwindConfig = {
#'        theme: {
#'            extend: {
#'                colors: {
#'                    'daisy': {
#'                        '50': '#fefcf6',
#'                        '100': '#fdfaec',
#'                        '200': '#faf2d0',
#'                        '300': '#f7e9b4',
#'                        '400': '#f1d97c',
#'                        '500': '#EBC944',
#'                        '600': '#d4b53d',
#'                        '700': '#b09733',
#'                        '800': '#8d7929',
#'                        '900': '#736221'
#'                    }
#'                }
#'            },
#'        }
#'    }
#'
#'    // Refresh so tailwind JIT CDN can see the config
#'    window.tailwindCSS.refresh();
#'  ```
#'
#'
#'  ## Example config with Tailwind Typography and Tailwind Forms
#'  ```{js}
#'    // File: tailwind.config.js
#'
#'    // Load custom CDN modules
#'    import tailwindcssTypography from 'https://cdn.skypack.dev/@tailwindcss/typography';
#'    import tailwindcssForms from 'https://cdn.skypack.dev/@tailwindcss/forms';
#'
#'    // Define config file
#'    window.tailwindConfig = {
#'        plugins: [
#'            tailwindcssTypography,
#'            tailwindcssForms
#'        ]
#'    };
#'
#'    // Refresh so tailwind JIT CDN can see the config
#'    window.tailwindCSS.refresh();
#'
#'  ```
#'
#' @param css Optional. Path to ".css" file. Can use @apply tags from Tiny.
#' @param tailwindConfig Optional. Path to ".js" file containing configuration.
#'  Requires two things:
#'  1. The json config needs to be defined as `window.tailwindConfig = {...}`
#'  2. After defining the config, use `window.tailwindCSS.refresh();`
#'  See details section `Custom Tailwind Config`for more details on format.
#'
#' @export
use_tailwind = function(css = NULL, tailwindConfig = NULL) {

    # Check files exists
    if(!is.null(css)) {
        for(i in seq_along(css)) {
            if(!file.exists(css[i])) {
                stop(glue::glue("File: {css[i]} doesn't exist"))
            }
        }
    }
    if(!is.null(tailwindConfig)) {
        if(!file.exists(tailwindConfig)) {
            stop(glue::glue("File: {tailwindConfig} doesn't exist"))
        }
    }

    # Initialize html elements

    # tailwindcss-jit
    html_cdn <- list(htmltools::HTML("<!-- Include CDN JavaScript -->\n<script src='https://unpkg.com/tailwindcss-jit-cdn'></script>"))

    html_css <- NULL
    html_config <- NULL

    # Custom CSS
    # Requires a css file (no style tags!)
    if(!is.null(css)) {
        html_css <- lapply(css, function(x) {
            htmltools::HTML(
                paste0(
                    "<style type='postcss'>\n\n",
                    paste0(xfun::read_utf8(x), collapse = "\n"),
                    "\n\n</style>",
                    collapse = "\n"
                )
            )
        })
    }

    # Custom config
    # Requires:
    #   1. window.tailwindConfig = json object of config
    #   2. call window.tailwindCSS.refresh();
    if(!is.null(tailwindConfig)) {
        html_config <- list(
            htmltools::HTML(
                paste0(
                    "<!-- Specify a custom TailwindCSS configuration -->\n",
                    "<script type='module'>\n\n",
                    paste0(xfun::read_utf8(tailwindConfig), collapse = "\n"),
                    "\n\n</script>"
                )
            ),
            htmltools::HTML(
                "<script type='tailwind-config'>\n window.tailwindConfig\n</script>"
            )
        )
    }

    htmltools::tagList(
        c(
            html_cdn,
            html_config,
            html_css
        )
    )
}
