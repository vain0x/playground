// サーバーサイド

// 商品データの取得。
// 実際にはデータベース等を使うが、
// ここではシンプルにハードコードしておく。

const ITEMS = []

for (let i = 1; i <= 999; i++) {
  ITEMS.push({
    itemId: i,
    title: `ITEM ${i}`,
  })
}

const find = q => {
  if (!q) {
    return ITEMS
  }

  return ITEMS.filter(item => item.title.includes(q))
}

module.exports = find
