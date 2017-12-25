using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.IO
{
    /// <summary>
    /// Represents a file or a directory.
    /// <para lang="ja">
    /// ファイルまたはディレクトリーを表す。
    /// </para>
    /// </summary>
    public abstract class FileObject
    {
        /// <summary>
        /// Gets the name.
        /// <para lang="ja">
        /// 名前を取得する。
        /// </para>
        /// </summary>
        public abstract string Name { get; }

        /// <summary>
        /// Gets the file path to this.
        /// <para lang="ja">
        /// ファイルパスを取得する。
        /// </para>
        /// </summary>
        public abstract FilePath Path { get; }

        /// <summary>
        /// Gets a value indicating whether this exists on the disk.
        /// <para lang="ja">
        /// これが存在するかを取得する。
        /// </para>
        /// </summary>
        /// <returns></returns>
        public abstract bool Exists();

        /// <summary>
        /// Gets or sets the creation time.
        /// <para lang="ja">
        /// 作成日時を取得・設定する。
        /// </para>
        /// </summary>
        public abstract DateTimeOffset CreationTime { get; set; }

        /// <summary>
        /// Gets or sets the time this was last accessed.
        /// <para lang="ja">
        /// 最終アクセス日時を取得・設定する。
        /// </para>
        /// </summary>
        public abstract DateTimeOffset LastAccessTime { get; set; }

        /// <summary>
        /// Gets or sets the time this was last written to.
        /// <para lang="ja">
        /// 最終更新日時を取得・設定する。
        /// </para>
        /// </summary>
        public abstract DateTimeOffset LastWriteTime { get; set; }
    }
}
