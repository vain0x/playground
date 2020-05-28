using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Text;
using System.Threading.Tasks;
using Reactive.Bindings;
using VainZero.Dancehall.Reactive.Bindings;

namespace VainZero.Dancehall.Fronts
{
    public abstract class Playable
        : IDisposable
    {
        public abstract bool IsAvailable { get; }
        public abstract string Title { get; }
        public abstract TimeSpan Duration { get; }
        public abstract TimeSpan CurrentTime { get; set; }
        public abstract void Play();
        public abstract void Pause();
        public abstract void Dispose();
    }

    public sealed class DemoPlayable
        : Playable
    {
        const string path = "data/secret-music.m4a";

        readonly TagLib.File file;
        readonly NAudio.Wave.AudioFileReader reader;
        readonly NAudio.Wave.WaveOut waveOut;

        public override string Title { get; }
        public override TimeSpan Duration { get; }

        public override TimeSpan CurrentTime
        {
            get { return reader.CurrentTime; }
            set { reader.CurrentTime = value; }
        }

        public override bool IsAvailable => System.IO.File.Exists(path);

        public override void Play()
        {
            waveOut.Play();
        }

        public override void Pause()
        {
            waveOut.Pause();
        }

        public override void Dispose()
        {
            file.Dispose();
            reader.Dispose();
            waveOut.Dispose();
        }

        public DemoPlayable()
        {
            file = TagLib.File.Create(path);
            reader = new NAudio.Wave.AudioFileReader(path);

            waveOut =
                new NAudio.Wave.WaveOut()
                {
                    Volume = 0.2F
                };
            waveOut.Init(reader);

            var tag = file.Tag;
            Title = tag.Title;
            Duration = file.Properties.Duration;
        }
    }

    public sealed class PlayControllerSlider
    {
        public TimeSpan Duration { get; }
        public ReadOnlyReactiveProperty<TimeSpan> ElapsedTime { get; }

        public double TotalSeconds => Duration.TotalSeconds;
        public ReactiveProperty<double> ElapsedSeconds { get; }

        public PlayControllerSlider(TimeSpan duration)
        {
            Duration = duration;
            ElapsedSeconds = 0.0.MakeReactiveProperty();

            ElapsedTime = ElapsedSeconds.Select(TimeSpan.FromSeconds).ToReadOnlyReactiveProperty();
        }
    }

    public sealed class PlayController
    {
        public ReactiveProperty<Playable> Current { get; }
        public ReadOnlyReactiveProperty<PlayControllerSlider> Slider { get; }

        public ReactiveCommand TogglePlayCommand { get; }

        public PlayController(Playable playable)
        {
            Current = playable.MakeReactiveProperty();

            Slider =
                Current
                .Select(p => new PlayControllerSlider(p.Duration))
                .ToReadOnlyReactiveProperty();

            Slider
                .Select(slider => slider.ElapsedTime)
                .Switch()
                .Throttle(TimeSpan.FromMilliseconds(500))
                .Subscribe(elapsedTime =>
                {
                    Current.Value.CurrentTime = elapsedTime;
                });

            TogglePlayCommand = new ReactiveCommand();

            TogglePlayCommand.Subscribe(_ =>
            {
                playable.Play();
            });
        }
    }
}
