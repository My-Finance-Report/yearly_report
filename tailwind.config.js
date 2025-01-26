/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./src/**/*.hs",
  ],
  theme: {
    extend: {
      colors:{
        "primary": "#415D43",
        "primary-alt": "#709775",
        "dark": "#111D13",
        "light": "#A1CCA5"
      }
    },
  },
  plugins: [],
}

