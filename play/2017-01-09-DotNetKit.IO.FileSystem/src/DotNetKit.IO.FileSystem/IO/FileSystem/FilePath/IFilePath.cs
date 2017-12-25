using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DotNetKit.ErrorHandling;

namespace DotNetKit.IO
{
    /// <summary>
    /// Represents a path, relative or absolute, to a file or directory.
    /// <para lang="ja">
    /// ファイルまたはディレクトリーへの絶対パスまたは相対パスを表す。
    /// </para>
    /// </summary>
    public interface IFileObjectPath
    {
        /// <summary>
        /// Gets the parent directory.
        /// <para lang="ja">
        /// 親ディレクトリーを取得する。
        /// </para>
        /// </summary>
        IDirectoryPath Parent { get; }
    }

    /// <summary>
    /// Represents an absolute path to a file or a directory.
    /// <para lang="ja">
    /// ファイルまたはディレクトリーへの絶対パスを表す。
    /// </para>
    /// </summary>
    public interface IAbsoluteFileObjectPath
        : IFileObjectPath
    {
        /// <summary>
        /// Gets a full path.
        /// <para lang="ja">
        /// 完全パスを取得する。
        /// </para>
        /// </summary>
        string FullPath(string separator);

        /// <summary>
        /// Gets the name of the file or directory.
        /// <para lang="ja">
        /// ファイルまたはディレクトリーの名前を取得する。
        /// </para>
        /// </summary>
        string Name { get; }

        /// <summary>
        /// Gets the extension.
        /// <para lang="ja">
        /// 拡張子を取得する。
        /// </para>
        /// </summary>
        string Extension { get; }

        IRelativeFileObjectPath ToRelativePath(IAbsoluteDirectoryPath basePath);

        new Option<IAbsoluteDirectoryPath> Parent { get; }
    }

    /// <summary>
    /// Represents a relative path to a file or a directory.
    /// <para lang="ja">
    /// ファイルまたはディレクトリーへの相対パスを表す。
    /// </para>
    /// </summary>
    public interface IRelativeFileObjectPath
        : IFileObjectPath
    {
        IAbsoluteFileObjectPath ToAbsolutePath(IAbsoluteDirectoryPath basePath);

        new IRelativeDirectoryPath Parent { get; }
    }

    /// <summary>
    /// Represents a path, relative or absolute, to a directory.
    /// <para lang="ja">
    /// ディレクトリーへの絶対または相対パスを表す。
    /// </para>
    /// </summary>
    public interface IDirectoryPath
        : IFileObjectPath
    {
        IFileObjectPath GetChild(string name);
        IFilePath GetChildFile(string name);
        IDirectoryPath GetChildDirectory(string name);
    }

    /// <summary>
    /// Represents an absolute path to a directory.
    /// <para lang="ja">
    /// ディレクトリーへの絶対パスを表す。
    /// </para>
    /// </summary>
    public interface IAbsoluteDirectoryPath
        : IDirectoryPath
        , IAbsoluteFileObjectPath
    {
        new IAbsoluteFileObjectPath GetChild(string name);
        new IAbsoluteFilePath GetChildFile(string name);
        new IAbsoluteDirectoryPath GetChildDirectory(string name);

        new IRelativeDirectoryPath ToRelativePath(IAbsoluteDirectoryPath basePath);
    }

    /// <summary>
    /// Represents a relative path to a directory.
    /// <para lang="ja">
    /// ディレクトリーへの相対パスを表す。
    /// </para>
    /// </summary>
    public interface IRelativeDirectoryPath
        : IDirectoryPath
        , IRelativeFileObjectPath
    {
        new IRelativeFileObjectPath GetChild(string name);
        new IRelativeFilePath GetChildFile(string name);
        new IRelativeDirectoryPath GetChildDirectory(string name);

        new IAbsoluteDirectoryPath ToAbsolutePath(IAbsoluteDirectoryPath basePath);
    }

    /// <summary>
    /// Represents a path, relative or absolute, to a file.
    /// <para lang="ja">
    /// ファイルへの絶対または相対パスを表す。
    /// </para>
    /// </summary>
    public interface IFilePath
        : IFileObjectPath
    {
    }

    /// <summary>
    /// Represents an absolute path to a file.
    /// <para lang="ja">
    /// ファイルへの絶対パスを表す。
    /// </para>
    /// </summary>
    public interface IAbsoluteFilePath
        : IFilePath
        , IAbsoluteFileObjectPath
    {
        new IRelativeFilePath ToRelativePath(IAbsoluteDirectoryPath basePath);
    }

    /// <summary>
    /// Represents a relative path to a file.
    /// <para lang="ja">
    /// ファイルへの相対パスを表す。
    /// </para>
    /// </summary>
    public interface IRelativeFilePath
        : IFilePath
        , IAbsoluteFileObjectPath
    {
        new IAbsoluteFilePath ToAbsolutePath(IAbsoluteDirectoryPath basePath);
    }
}
