// Custom CDN modules
import tailwindcssTypography from 'https://cdn.skypack.dev/@tailwindcss/typography';
import tailwindcssForms 	 from 'https://cdn.skypack.dev/@tailwindcss/forms';

// Define config file
window.tailwindConfig = {
	plugins: [
		tailwindcssTypography,
		tailwindcssForms
	]
};

// Refresh so tailwind JIT CDN can see the config
window.tailwindCSS.refresh();
