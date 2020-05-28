import * as fs from "fs";
import * as os from "os";
import * as path from "path";

export interface IObjectRepository<T> {
  load(): Promise<T | undefined>;
  save(value: T): Promise<void>;
}

export interface IConfigRepository {
  make<T>(key: string): IObjectRepository<T>;
}

export class LocalObjectRepository<T> implements IObjectRepository<T> {
  constructor(
    private readonly rootDir: string,
    private readonly key: string,
  ) {
  }

  get localFilePath() {
    return path.join(this.rootDir, "var", this.key + ".local.json");
  }

  get templateFilePath() {
    return path.join(this.rootDir, "var", this.key + ".template.json");
  }

  exists(filePath: string) {
    return new Promise<boolean>(resolve => fs.exists(filePath, exists => resolve(exists)));
  }

  async loadCore(filePath: string) {
    return new Promise<T>((resolve, reject) => {
      fs.readFile(filePath, {}, (err, data) => {
        if (err) return reject(err);
        resolve(JSON.parse(data.toString()) as T);
      });
    });
  }

  async load(): Promise<T | undefined> {
    const localExists = await this.exists(this.localFilePath);
    if (localExists) {
      return await this.loadCore(this.localFilePath);
    }

    const templateExists = await this.exists(this.templateFilePath);
    if (templateExists) {
      return this.loadCore(this.templateFilePath);
    }

    return Promise.resolve(undefined);
  }

  async save(value: T): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      fs.writeFile(this.localFilePath, JSON.stringify(value, undefined, "  "), {}, err => {
        if (err) return reject(err);
        resolve();
      });
    });
  }
}

export const ConfigRepository = new class {
  create(rootDir: string): IConfigRepository {
    return new class implements IConfigRepository {
      constructor(private rootDir: string) {
      }

      make<T>(key: string): IObjectRepository<T> {
        return new LocalObjectRepository<T>(rootDir, key);
      }
    }(rootDir);
  }
};
