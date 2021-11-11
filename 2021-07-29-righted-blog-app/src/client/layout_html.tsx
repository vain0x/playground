import React, { ReactNode } from "react"
import { renderToString } from "react-dom/server"
import { Main, MainProps } from "./main"

interface HtmlProps {
  title: string
  head: ReactNode
  children: ReactNode
}

const Html = ({ title, head, children }: HtmlProps) => (
  <html lang="ja">
    <head>
      <meta charSet="utf-8" />
      <meta httpEquiv="X-UA-Compatible" content="IE=edge" />
      <title>{title}</title>
      <meta name="viewport" content="width=device-width, initial-scale=1" />
      <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.0/normalize.min.css" integrity="sha256-oSrCnRYXvHG31SBifqP2PM1uje7SJUyX0nTwO2RJV54=" crossOrigin="anonymous" />
      <link rel="stylesheet" href="/static/sakura.css" type="text/css" />
      {/* <link rel="stylesheet" type="text/css" media="screen" href="/static/theme.css" /> */}
      <script src="https://unpkg.com/react@17.0.2/umd/react.development.js" crossOrigin="anonymous" />
      <script src="https://unpkg.com/react-dom@17.0.2/umd/react-dom.development.js" crossOrigin="anonymous" />
      <script src="/static/bundle.js" defer />
      {head}
    </head>

    <body>
      <article id="app-container">
        {children}
      </article>
    </body>
  </html>
)

export const renderMainHtmlToString = (title: string, props: MainProps) =>
  `<!DOCTYPE html>\n${renderToString(
    <Html
      title={title}
      head={(
        <script id="main-props-script" data-json={JSON.stringify(props)} />
      )}>
      <Main {...props} />
    </Html>
  )}`
