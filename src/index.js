(() => {
  const patch = (parent, xe, ye) => {
    // console.debug({ patch: "", xe, ye })

    if (!xe || !ye) {
      console.error("bug?")
      return
    }

    if (xe.nodeName === ye.nodeName) {
      // Update attributes:

      const xa = new Set(xe.getAttributeNames())
      const ya = new Set(ye.getAttributeNames())

      for (const a of xa) {
        if (ya.has(a)) {
          const v = ye.getAttribute(a)
          if (xe.getAttribute(a) !== v) {
            // console.debug({ attrSet: a, v })
            xe.setAttribute(a, v)
          }
          continue
        }

        // console.debug({ attrDel: a, v: xe.getAttribute(a) })
        xe.removeAttribute(a)
      }

      for (const a of ya) {
        if (xa.has(a)) {
          continue
        }

        // console.debug({ attrAdd: a, v: ye.getAttribute(a) })
        xe.setAttribute(a, ye.getAttribute(a))
      }

      // Update children:

      let xi = 0
      let yi = 0
      while (xi < xe.children.length && yi < ye.children.length) {
        patch(xe, xe.children[xi], ye.children[yi])
        xi++
        yi++
      }

      while (yi < ye.children.length) {
        const y = document.importNode(ye.children[yi].cloneNode(true), true)
        // console.debug({ insert: y })
        xe.insertBefore(y, null)
        xi++
        yi++
      }

      while (xi < xe.children.length) {
        // console.debug({ remove: xe.children[xi] })
        xe.removeChild(xe.children[xi])
        // xi++
      }

      if (xe.children.length === 0 && xe.textContent !== ye.textContent) {
        // console.debug({ text: ye.textContent })
        xe.textContent = ye.textContent
      }
      return
    }

    // console.debug({ replace: xe, with: ye })
    parent.insertBefore(document.importNode(ye.cloneNode(true), true), xe)
    parent.removeChild(xe)
  }

  window.HydrateJs = {
    hydrate: html => {
      // console.log("hydrate")

      const shadow = document.implementation.createHTMLDocument("shadow")
      shadow.write(html)

      patch(document.body.parentNode, document.body, shadow.body)
    },
  }
})()
