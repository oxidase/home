#include <QCoreApplication>
#include <QDebug>
#include <QAudioInput>
#include <QAudioOutput>
#include <QAudioDeviceInfo>
#include <QBuffer>
#include <QFile>

class CircularBuffer : public QIODevice
{
    Q_OBJECT
    QByteArray arr;
    int frameSize;
    int pollTimer;
public:
    CircularBuffer(QObject * parent = 0) :
        QIODevice(parent), frameSize(100)
    {
        pollTimer = startTimer(0);
    }

    ~CircularBuffer()
    {
        killTimer(pollTimer);
    }

    qint64 bytesAvailable() const
    {
        //return (arr.length() / frameSize) * frameSize;
        return frameSize;
    }

protected:
    void timerEvent(QTimerEvent *timerEvent)
    {
        if (timerEvent->timerId() == pollTimer)
            emit readyRead();

        QIODevice::timerEvent(timerEvent);
    }

    qint64 readData(char *data, qint64 maxlen)
    {
        size_t len = qMin(maxlen, (qint64)arr.length());
        if (len) {
            if (data)
                memcpy(data, arr.constData(), len);
            arr.remove(0, len);
            qDebug() << "readData =>" <<  arr.length() << len;
        }
        // toxav_recv_audio
        return len;
    }
    qint64 readLineData(char *data, qint64 maxlen)
    {
        Q_UNREACHABLE();
        return 0;
    }
    qint64 writeData(const char *data, qint64 len)
    {
        // toxav_prepare_audio_frame
        // toxav_send_audio
        arr += QByteArray::fromRawData(data, len);
        qDebug() << "writeData" << len << arr.length();
        emit bytesWritten(len);
        if (arr.length() >= frameSize)
            emit readyRead();
        return len;
    }
};

int main(int argc, char *argv[]) {
    QCoreApplication app(argc, argv);

    foreach (const QAudioDeviceInfo &deviceInfo, QAudioDeviceInfo::availableDevices(QAudio::AudioInput))
        qDebug() << "AudioInput device name: " << deviceInfo.deviceName()
                 << deviceInfo.supportedChannelCounts()
                 << deviceInfo.supportedCodecs()
                 << deviceInfo.supportedSampleRates()
                 << deviceInfo.supportedSampleSizes()
                 << deviceInfo.supportedSampleTypes();

    foreach (const QAudioDeviceInfo &deviceInfo, QAudioDeviceInfo::availableDevices(QAudio::AudioOutput))
        qDebug() << "AudioOutput device name: " << deviceInfo.deviceName()
                 << deviceInfo.supportedChannelCounts()
                 << deviceInfo.supportedCodecs()
                 << deviceInfo.supportedSampleRates()
                 << deviceInfo.supportedSampleSizes()
                 << deviceInfo.supportedSampleTypes();



    QAudioFormat format;
    // Set up the desired format, for example:
    format.setSampleRate(8000);
    format.setChannelCount(1);
    format.setSampleSize(8);
    format.setCodec("audio/pcm");
    format.setByteOrder(QAudioFormat::LittleEndian);
    format.setSampleType(QAudioFormat::UnSignedInt);

    QAudioDeviceInfo info = QAudioDeviceInfo::defaultInputDevice();
    if (!info.isFormatSupported(format)) {
        qWarning() << "Default format not supported, trying to use the nearest.";
        format = info.nearestFormat(format);
    }

    QAudioInput* input = new QAudioInput(format);
    QAudioOutput* output = new QAudioOutput(format);
    QObject::connect(input, &QAudioInput::stateChanged, [](QAudio::State state){ qDebug() << "input state" << state; });
    QObject::connect(output, &QAudioOutput::stateChanged, [&](QAudio::State state){ qDebug() << "output state" << state << " " << output->error(); });

    // QByteArray inputArray(1000, 0);
    // QBuffer inputBuffer(&inputArray);
    CircularBuffer buffer;
    // inputBuffer.open(QIODevice::ReadWrite | QIODevice::Unbuffered);
    buffer.open(QIODevice::ReadWrite | QIODevice::Unbuffered);
    input->start(&buffer);
    output->start(&buffer);

    // QObject::connect(&buffer, &QIODevice::bytesWritten, [](qint64 bytes){ qDebug() << "bytesWritten" << bytes; });
    // QObject::connect(&buffer, &QIODevice::readyRead, [&](){ qDebug() << "readyRead" << buffer.bytesAvailable(); buffer.read(0, buffer.bytesAvailable()); qDebug() << "        ";  });

    return app.exec();
}

#include "main.moc"
