export type Cmd<Msg> = () => Msg | undefined | Promise<Msg | undefined>

export type UpdateFn<Model, Msg> = (model: Model, msg: Msg) => [Model, Cmd<Msg>]

export type ViewFn<Model> = (model: Model) => unknown

export interface Program<Model, Msg> {
  model: Model,
  cmd: Cmd<Msg>,
  update: UpdateFn<Model, Msg>,
  view: ViewFn<Model>,
}
